(import (rnrs)
	(sagittarius socket)
	(util concurrent)
	(getopt))

(define (make-dump-task out)
  (lambda (recv send)
    (let loop ()
      (let ((msg (recv)))
	(call-with-port (open-file-output-port out
			  (file-options no-fail no-truncate append)
			  (buffer-mode block))
	  (lambda (out)
	    (put-u8 out (char->integer (car msg)))
	    (put-bytevector out (cadr msg))
	    (put-bytevector out (cddr msg)))))
      (loop))))

(define (read-command socket)
  (define (compute-size header)
    (- (bytevector-u32-ref header 0 (endianness little)) 16))
  (define (ensure-recv socket size)
    (let loop ((count size) (r '()))
      (let ((bv (socket-recv socket count)))
	(cond ((= (bytevector-length bv) count)
	       (bytevector-concatenate (reverse (cons bv r))))
	      ((zero? (bytevector-length bv))
	       (bytevector-concatenate (reverse r)))
	      (else (loop (- count (bytevector-length bv)) (cons bv r)))))))
  (let ((header (ensure-recv socket 16)))
    (if (not (= (bytevector-length header) 16))
	(values #f #f)
	(let ((body (ensure-recv socket (compute-size header))))
	  (values header body)))))

(define (start-server out port mongo-port)
  (define actor
    (actor-start! (make-shared-queue-channel-actor (make-dump-task out))))
  (define server-socket (make-server-socket port))
  (define client-socket (make-client-socket "localhost" mongo-port))
  (define (done socket)
    (define (close socket)
      (socket-shutdown socket SHUT_RDWR)
      (socket-close socket))
    (close socket)
    (close client-socket)
    (close server-socket))

  (let ((socket (socket-accept server-socket)))
    (let loop ()
      (define (forward socket client-head client-body)
	(actor-send-message! actor (cons* #\C client-head client-body))
	(socket-send client-socket client-head)
	(socket-send client-socket client-body)
	(let-values (((server-head server-body) (read-command client-socket)))
	  (actor-send-message! actor (cons* #\S server-head server-body))
	  (when server-head
	    (socket-send socket server-head)
	    (socket-send socket server-body)))
	  (loop))
      (let-values (((client-head client-body) (read-command socket)))
	(if client-head
	    (forward socket client-head client-body)
	    (done socket))))))

(define (main args)
  (with-args (cdr args)
      ((out (#\o "out") #t "proxy.out")
       (port (#\p "port") #t "27017")
       (mongo-port (#\m "mongodb-port") #t (usage))
       . rest)
    (start-server out port mongo-port)))
