#!r6rs
(import (rnrs)
	(mongodb connection)
	(mongodb conditions)
	(srfi :64)
	(srfi :98))

(test-begin "Mongodb connection")

(test-assert "make connection 1"
	     (mongodb-connection? (make-mongodb-connection "localhost")))
(test-assert "make connection 2"
	     (mongodb-connection? (make-mongodb-connection "localhost" 27017)))
(test-error "make connection 4"
	    assertion-violation?
	    (mongodb-connection?
	     (make-mongodb-connection "localhost" 27017 1)))

(define option
  (make-mongodb-connection-option
   (lambda (conn database?) 1)
   values
   #t))
  
(when (get-environment-variable "MONGODB_RUNNING")
  (let ((conn (make-mongodb-connection "localhost" 27017 option)))
    (test-assert "open" (mongodb-connection? (open-mongodb-connection! conn)))
    (test-assert "opened?" (mongodb-connection-open? conn))
    (test-assert "input-port?"
		 (input-port? (mongodb-connection-input-port conn)))
    (test-assert "output-port?"
		 (output-port? (mongodb-connection-output-port conn)))
    (test-assert "list-databases"
		 (member "admin" (mongodb-connection-list-databases conn)))
    (test-assert "close"
		 (mongodb-connection? (close-mongodb-connection! conn)))))

(let ((conn (make-mongodb-connection "localhost")))
  (test-assert "opened? 2" (not (mongodb-connection-open? conn)))
  (test-error "input-port 2"
	      mongodb-error? (mongodb-connection-input-port conn))
  (test-error "output-port 2"
	      mongodb-error? (mongodb-connection-output-port conn)))

(test-end)
