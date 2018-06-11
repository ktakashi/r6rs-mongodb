(import (rnrs)
        (mongodb)
	(mongodb authenticate)
	(srfi :64)
	(srfi :98))

(unless (get-environment-variable "MONGODB_RUNNING") (exit 0))

(define connection 
  (open-mongodb-connection! (make-mongodb-connection "localhost")))
(define database (make-mongodb-database connection "test"))

(define collection "testCollection")

(test-begin "MongoDB authenticate")

(mongodb-database-run-command database
 '(("createUser" "user")
   ("pwd" "password")
   ("roles" #())))

(test-error mongodb-authenticate-error?
	    (mongodb-database-authenticate! database "user" "invalid"))
(test-assert (mongodb-database-authenticate! database "user" "password"))

(mongodb-database-run-command database '(("dropUser" "user")))

(test-end)

(close-mongodb-connection! connection)

