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
      ("name" "name1")
      ("data" "some data")))
  (list->vector (do ((i 0 (+ i 1)) (r '() (cons (gen i) r)))
		    ((= i count) r))))
(define (strip-default-id vec)
  (list->vector
   (map (lambda (s) (remp (lambda (s) (string=? "_id" (car s))) s))
	(vector->list vec))))

(when (get-environment-variable "MONGODB_RUNNING")
  (let* ((conn (open-mongodb-connection! (make-mongodb-connection "localhost")))
	 (db (make-mongodb-database conn "test"))
	 (collection "testCollection"))
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

    (test-assert "insert many"
		 (mongodb-database-insert db collection
					  (create-test-data 10)))

    (test-equal "10 documents are inserted"
		10
		(vector-length
		 (mongodb-query-result-documents
		  (mongodb-database-query db collection '(("name" "name1"))))))
    (test-equal "7 documents are queried"
		7
		(vector-length
		 (mongodb-query-result-documents
		  (mongodb-database-query db collection '(("name" "name1"))
					  3))))
    (test-equal "4 documents are queried"
		4
		(vector-length
		 (mongodb-query-result-documents
		  (mongodb-database-query db collection '(("name" "name1"))
					  3 4))))
    ;; TODO cursor tests
    (let* ((r (mongodb-database-query db collection '(("name" "name1")) 0 3))
	   (cid (mongodb-query-result-cursor-id r)))
      (test-assert "cursor id 0"
		   (not (mongodb-database-get-more db collection 0)))
      (test-equal "get more 1"
		  4
		  (vector-length
		   (mongodb-query-result-documents
		    (mongodb-database-get-more db collection cid 4))))
      ;; default should be bigger than 3...
      (test-equal "get more 2"
		  3
		  (vector-length
		   (mongodb-query-result-documents
		    (mongodb-database-get-more db collection cid)))))
    
    (test-assert "update 1"
		 (mongodb-database-update db collection
					  '(("lang" "Scheme"))
					  '(("$set"
					     (("comment" "It's useful"))))))

    (test-equal "update query"
		'#((("name" "R6RS mongodb")
		    ("lang" "Scheme")
		    ("comment" "It's useful")))
		(strip-default-id
		 (mongodb-query-result-documents
		  (mongodb-database-query db collection '(("lang" "Scheme"))))))
    
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

    (test-assert (mongodb-database-delete db collection '(("name" "name1"))))
    (test-equal "1 record is removed"
		9
		(vector-length
		 (mongodb-query-result-documents
		  (mongodb-database-query db collection '(("name" "name1"))))))
    (test-assert (mongodb-database-delete-all db collection '(("name" "name1"))))
    (test-equal "all records are removed"
		0
		(vector-length
		 (mongodb-query-result-documents
		  (mongodb-database-query db collection '(("name" "name1"))))))
    
    (close-mongodb-connection! conn))

  (let* ((conn (open-mongodb-connection! (make-mongodb-connection "localhost")))
	 (db (make-mongodb-database conn "test"))
	 (collection "testCommandCollection"))
    (test-assert (mongodb-database-drop-collection db collection))
    (test-equal "Insert using command 1"
		'(("n" (s32 1)) ("ok" 1))
		(mongodb-database-insert-command
		 db collection
		 '#((("_id" 1)
		     ("name" "portable R6RS mongodb")
		     ("lang" "R6RS Scheme")
		     ("comment" "It's useful")))))
    (test-assert "Existing _id should"
		 (assoc "writeErrors"
			(mongodb-database-insert-command db collection
			 '#((("_id" 1)
			     ("name" "portable R6RS mongodb")
			     ("lang" "R6RS Scheme")
			     ("comment" "It's useful"))))))
    (let ((r (mongodb-database-insert-command db collection
	      '#((("_id" 1)
		  ("name" "portable R6RS mongodb")
		  ("lang" "R6RS Scheme")
		  ("comment" "It's useful"))
		 (("_id" 2)
		  ("name" "R6RS mongodb library")
		  ("lang" "Scheme")
		  ("comment" "It's awesome!")))
	      '(ordered #f))))
      (test-equal "1 record is inserted"
		  '(s32 1) (cond ((assoc "n" r) => cadr)))
      (test-assert "one should be removed" (assoc "writeErrors" r)))
    ;; insert 50 records
    (mongodb-database-insert-command db collection (create-test-data 50))
    (test-equal "update command"
		;; FIXME order
		'(("n" (s32 2)) ("nModified" (s32 2)) ("ok" 1))
		(mongodb-database-update-command db collection
		 '#((("q" (("id" 1)))
		     ("u" (("$set" (("name" "updated1") ("info" "1"))))))
		    (("q" (("id" 2)))
		     ("u" (("$set" (("name" "updated2") ("info" "2")))))))))
    (let ((r (mongodb-database-update-command db collection
	      '#((("q" (("id" 1)))
		  ;; incorrect command
		  ("u" (("$ff" (("name" "updated1") ("info" "1"))))))
		 (("q" (("id" 2)))
		  ("u" (("$set" (("name" "updated2.0") ("info" "2")))))))
	      '(ordered #f))))
      (test-equal "update command not ordered (n)"
		  '(s32 1) (cond ((assoc "n" r) => cadr)))
      (test-equal "update command not ordered (nModified)"
		  '(s32 1) (cond ((assoc "nModified" r) => cadr)))
      (test-assert "update command not ordered (writeErrors)"
		   (assoc "writeErrors" r)))

    (let ((r (mongodb-database-delete-command db collection
	      '#((("q" (("id" 1))) ("limit" 0))
		 ;; oops incorrect command
		 (("q" (("id" (("$ff" 0))))) ("limit" 0))
		 (("q" (("id" (("$gt" 1))))) ("limit" 1)))
	      '(ordered #f))))
      (test-equal "delete command not ordered (n)"
		  '(s32 2) (cond ((assoc "n" r) => cadr)))
      (test-assert "delete command not ordered (writeErrors)"
		   (assoc "writeErrors" r)))
    
    (close-mongodb-connection! conn)))

(test-end)

