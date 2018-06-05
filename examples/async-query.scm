#|
This example shows how to make async query handler using low level APIs of
mongodb-database-send-query and mongodb-database-receive-reply

The example works only on Sagittarius Scheme.
|#
(import (rnrs)
        (mongodb connection)
	(mongodb database)
	(util concurrent)
	(srfi :117))

(define connection 
  (open-mongodb-connection! (make-mongodb-connection "localhost")))
(define database (make-mongodb-database connection "test"))

(define collection "testCollection")

;; Creates 3 actors working as if they are a pipeline.
;; Flow
;;   query-request-actor - (sends callback and request id)
;;                        \ 
;;                         +---- callback-consumer-actor
;;                        /
;;   receiving-actor     - (sends query-result)
(define query-request-actor
  (make-shared-queue-channel-actor
   (lambda (receiver sender)
     (let loop ()
       ;; (query-request database . callback)
       (let ((query-request (receiver)))
	 (if query-request
	     (let ((request-id ((car query-request))))
	       (actor-send-message! callback-consumer-actor
				    (cons request-id (cdr query-request)))
	       (loop))
	     (actor-send-message! receiving-actor #f)))))))

(define receiving-actor
  (make-shared-queue-channel-actor
   (lambda (receiver sender)
     (let loop ()
       (let ((query-result (mongodb-database-receive-reply database)))
	 (actor-send-message! callback-consumer-actor query-result)
	 (when (receiver 0.0 #t) (loop)))))))

(define callback-consumer-actor
  (make-shared-queue-channel-actor
   (lambda (receiver sender)
     (define callbacks (make-equal-hashtable))
     (define results (list-queue))
     (define (->key id database) (cons id (mongodb-database-name database)))
     (define (register-callback id database callback)
       (hashtable-set! callbacks (->key id database) callback))
     (define (find-callback query-result)
       (let ((database (mongodb-cursor-database query-result))
	     (id       (mongodb-query-result-to query-result)))
	 (cond ((hashtable-ref callbacks (->key id database)) =>
		(lambda (callback) (callback query-result)))
	       (else (list-queue-add-back! results query-result)))))
     (let loop ()
       (let ((msg (receiver)))
	 (when msg
	   (if (mongodb-query-result? msg)
	       (find-callback msg)
	       (let ((request-id (car msg))
		     (database (cadr msg))
		     (callback (cddr msg)))
		 (register-callback request-id database callback)))
	   (for-each (lambda (msg) (find-callback msg))
		     (list-queue-remove-all! results))
	   (loop)))))))

(define (mongodb-async-query database colletion query callback . options)
  (define number-to-skip (if (null? options) 0 (car options)))
  (define number-to-return (if (or (null? options) (null? (cdr options)))
			       0
			       (cadr options)))
  (actor-send-message! query-request-actor
		       (cons* (lambda ()
				(mongodb-database-send-query
				 database
				 (mongodb-database->namespace
				  database colletion)
				 number-to-skip number-to-return query #f))
			      database
			      callback)))

;; send finish message before receiving message
(actor-send-message! receiving-actor #f)

;; request query
(mongodb-async-query database collection '()
		     (lambda (doc) (display doc) (newline)))

;; finish query-request-actor
(actor-send-message! query-request-actor #f)

(actor-wait! query-request-actor)
(actor-wait! receiving-actor)

;; send finish message to callback-consumer-actor
(actor-send-message! callback-consumer-actor #f)
(actor-wait! callback-consumer-actor)

(close-mongodb-connection! connection)
