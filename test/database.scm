(import (rnrs)
	(mongodb connection)
	(mongodb database)
	(srfi :64)
	(srfi :98))

(test-begin "MongoDB database")

(test-error assertion-violation?
	    (make-mongodb-database (make-mongodb-connection "localhost")
				   "test"))

(when (get-environment-variable "MONGODB_RUNNING")
  (let ((conn (open-mongodb-connection! (make-mongodb-connection "localhost")))
	(db (make-mongodb-database conn "test")))
    (test-assert (mongodb-database? db))

    (close-mongodb-connection! conn)))

(test-end)

