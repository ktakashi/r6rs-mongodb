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

(define (create-test-data count)
  (define (gen i)
    `(("id" ,i)
      ("data" "some data")))
  (list->vector (do ((i 0 (+ i 1)) (r '() (cons (gen i) r)))
		    ((= i count) r))))

(when (get-environment-variable "MONGODB_RUNNING")
  (let* ((conn (open-mongodb-connection! (make-mongodb-connection "localhost")))
	 (db (make-mongodb-database conn "test"))
	 (collection "testCollection"))
    (define (strip-default-id vec)
      (list->vector
       (map (lambda (s) (remp (lambda (s) (string=? "_id" (car s))) s))
	    (vector->list vec))))
    (test-assert (mongodb-database? db))
    (test-equal '(("ok" 1)) (mongodb-database-run-command db '(("ping" 1))))
    (test-equal '(("ok" 1)) (mongodb-database-run-command db "ping"))
    (test-assert (assoc "databases"
			(mongodb-database-admin-command db "listDatabases")))
    (test-assert (mongodb-database-drop-collection db collection))
    (test-assert (mongodb-database-insert db collection
					  '#((("name" "R6RS mongodb")
					      ("lang" "Scheme")
					      ("comment" "It's useless")))))
    (test-equal '#((("name" "R6RS mongodb")
		    ("lang" "Scheme")
		    ("comment" "It's useless")))
		(strip-default-id
		 (mongodb-query-result-documents
		  (mongodb-database-query db collection '()))))

    (test-assert "insert 1"
		 (mongodb-database-update db collection
					  '(("lang" "Scheme"))
					  '(("$set"
					     (("comment" "It's useful"))))))
    (test-equal "insert query"
		'#((("name" "R6RS mongodb")
		    ("lang" "Scheme")
		    ("comment" "It's useful")))
		(strip-default-id
		 (mongodb-query-result-documents
		  (mongodb-database-query db collection '()))))
    
    (test-assert "upsert 1"
		 (mongodb-database-upsert db collection
					  '(("lang" "C"))
					  '(("$set"
					     (("lang" "C")
					      ("name" "Default driver?")
					      ("comment" "No idea"))))))
    (test-assert "upsert 2"
		 (mongodb-database-upsert db collection
					  '(("lang" "C"))
					  '(("$set"
					     (("lang" "C")
					      ("name" "Default driver?")
					      ("comment" "No idea"))))))
    ;; FIXME order might be different
    (test-equal "upsert query"
		'#((("lang" "C")
		    ("comment" "No idea")
		    ("name" "Default driver?")))
		(strip-default-id
		 (mongodb-query-result-documents
		  (mongodb-database-query db collection '(("lang" "C"))))))
    
    (close-mongodb-connection! conn)))

(test-end)

