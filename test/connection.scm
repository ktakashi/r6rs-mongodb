#!r6rs
(import (rnrs)
	(mongodb connection)
	(mongodb conditions)
	(srfi :64)
	(srfi :98))

(test-begin "Mongodb connection")

(test-assert (mongodb-connection? (make-mongodb-connection "localhost")))
(test-assert (mongodb-connection? (make-mongodb-connection "localhost" 27017)))
(test-assert (mongodb-connection?
	      (make-mongodb-connection "localhost" 27017 #f)))
(test-error assertion-violation?
	    (mongodb-connection?
	     (make-mongodb-connection "localhost" 27017 1)))

(define (strategy conn database?) 1)
  
(when (get-environment-variable "MONGODB_RUNNING")
  (let ((conn (make-mongodb-connection "localhost" 27017 strategy)))
    (test-assert (mongodb-connection? (open-mongodb-connection! conn)))
    (test-assert (mongodb-connection-open? conn))
    (test-assert (input-port? (mongodb-connection-input-port conn)))
    (test-assert (output-port? (mongodb-connection-output-port conn)))
    (test-assert (assoc "databases" (mongodb-connection-list-databases conn)))
    (test-assert (mongodb-connection? (close-mongodb-connection! conn)))))

(let ((conn (make-mongodb-connection "localhost")))
  (test-assert (not (mongodb-connection-open? conn)))
  (test-error mongodb-error? (mongodb-connection-input-port conn))
  (test-error mongodb-error? (mongodb-connection-output-port conn)))

(test-end)
