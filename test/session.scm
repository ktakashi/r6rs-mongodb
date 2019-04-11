#!r6rs
(import (rnrs)
	(mongodb session)
	(mongodb connection)
	(mongodb database)
	(srfi :64)
	(srfi :98))

(test-begin "Session")

(let ((conn (make-mongodb-connection "localhost")))
  (test-assert "start session 1"
	       (mongodb-session? (mongodb-session-start conn)))
  (test-error "start session 2" assertion-violation?
	      (mongodb-session-start 'connection)))

(define option
  (make-mongodb-connection-option
   (lambda (conn database?) 1)
   values
   #t))

(define (ok? r)
  (cond ((assoc "ok" r) => (lambda (s) (> (cadr s) 0)))
	(else #f)))

(define conn (make-mongodb-connection "localhost" 27017 option))
(define collection "transactionalCollection")

(define (recreaate conn)
  ;; ignore erros here
  (let ((db (make-mongodb-database conn "test")))
    (mongodb-database-drop-collection db collection)
    (mongodb-database-run-command db `(("create" ,collection)))))

(when (get-environment-variable "MONGODB_RUNNING")
  (open-mongodb-connection! conn)
  ;; Commit
  (recreaate conn)
  (let* ((session (mongodb-session-start conn))
	 (db (mongodb-session-database session "test")))
    (test-assert "Session database" (mongodb-database? db))
    (test-assert "Session id" (string? (mongodb-session-id session)))
    (guard (e (else (test-assert "Unexpected error" #f) ))
      (mongodb-session-start-transaction! session)
      (test-assert "Insert 1"
		   (ok? (mongodb-database-insert-command db collection
			 '#((("id" 1) ("foo" "bar"))))))
      (test-assert "Insert 2"
		   (ok? (mongodb-database-insert-command db collection
			 '#((("id" 2) ("foo" "bar"))))))
      (test-assert "Commit"
		   (ok? (mongodb-session-commit-transaction! session)))
      (test-assert "Session end" (ok? (mongodb-session-end! session))))
    (when (mongodb-session-transaction-supported? session)
      (let* ((db (make-mongodb-database conn "test"))
	     (r (mongodb-database-query db collection '())))
	(test-equal "Inserted documents" 2
		  (vector-length (mongodb-query-result-documents r))))))

  ;; Abort
  (recreaate conn)
  (let* ((session (mongodb-session-start conn))
	 (db (mongodb-session-database session "test")))
    (test-assert "Session database" (mongodb-database? db))
    (test-assert "Session id" (string? (mongodb-session-id session)))
    (guard (e (else (test-assert "Unexpected error" #f)))
      (mongodb-session-start-transaction! session)
      (test-assert "Insert 3"
		   (ok? (mongodb-database-insert-command db collection
			 '#((("id" 1) ("foo" "bar"))))))
      (test-assert "Insert 4"
		   (ok? (mongodb-database-insert-command db collection
			 '#((("id" 2) ("foo" "bar"))))))
      (test-assert "Abort"
		   (ok? (mongodb-session-abort-transaction! session)))
      (test-assert "Session end" (ok? (mongodb-session-end! session))))
    (when (mongodb-session-transaction-supported? session)
      (let* ((db (make-mongodb-database conn "test"))
	     (r (mongodb-database-query db collection '())))
	(test-equal "Inserted documents" '#()
		    (mongodb-query-result-documents r)))))
  )


(test-end)
