(import (rnrs)
	(mongodb connection)
	(mongodb database)
	(srfi :64)
	(srfi :98))

(test-begin "MongoDB database")

(test-assert (mongodb-database?
	      (make-mongodb-database (make-mongodb-connection "localhost")
				     "test")))
(let ((db (make-mongodb-database (make-mongodb-connection "localhost")
				 "test")))
  (test-error mongodb-connection-closed?
	      (mongodb-database-run-command db '(("getLastError" 1)))))

(when (get-environment-variable "MONGODB_RUNNING")
  (let* ((conn (open-mongodb-connection! (make-mongodb-connection "localhost")))
	 (db (make-mongodb-database conn "test")))
    (test-assert (mongodb-database? db))
    (test-equal '(("ok" 1)) (mongodb-database-run-command db '(("ping" 1))))
    (test-equal '(("ok" 1)) (mongodb-database-run-command db "ping"))
    (test-assert (assoc "databases"
			(mongodb-database-admin-command db "listDatabases")))
    (close-mongodb-connection! conn)))

(test-end)

