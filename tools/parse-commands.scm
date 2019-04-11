(import (rnrs)
	(mongodb protocol)
	(pp))

(define (dump-msg-header header)
  ;; nothing...
  )

(define (dump-op-msg msg)
  (display "OP_MSG #b")
  (display (number->string (op-msg-flag-bits msg) 2))
  (display ", # of sections ")
  (display (vector-length (op-msg-sections msg))) (newline)
  (dump-msg-header (mongodb-protocol-message-header msg))
  (pp (op-msg-sections msg)))

(define (dump-op-query msg)
  (display "OP_QUERY") (newline)
  (dump-msg-header (mongodb-protocol-message-header msg))
  (pp (op-query-query msg)))

(define (dump-op-reply msg)
  (display "OP_REPLY") (newline)
  (dump-msg-header (mongodb-protocol-message-header msg))
  (pp (op-reply-documents msg)))

(define (read&dump in)
  (define (dump type msg)
    (display (integer->char type)) (display " ")
    (cond ((op-msg? msg) (dump-op-msg msg))
	  ((op-query? msg) (dump-op-query msg))
	  ((op-reply? msg) (dump-op-reply msg))
	  (else (pp msg))))
  
  (let loop ()
    (unless (eof-object? (lookahead-u8 in))
      (let ((type (get-u8 in)))
	(dump type (read-mongodb-message in))
	(newline)
	(loop)))))

(define (main args)
  (let ((file (cadr args)))
    (call-with-input-file file read&dump :transcoder #f)))
