#!r6rs
(import (rnrs)
	(rnrs mutable-pairs)
	(mongodb)
	(srfi :64)
	(srfi :98))

(test-begin "MongoDB")

;; sanity check
(test-assert call-with-mongodb-connection)
(test-assert call-with-mongodb-database)

;; Chez doesn't like to have 2 connections in one session.
;; Maybe the socket binding issue.
;; 
;; (call-with-mongodb-connection "localhost" 27017
;;  (lambda (conn)
;;    (test-assert (mongodb-connection? conn))
;;    (test-assert (mongodb-connection-open? conn))))

(define collection "mongodbTestCollection")
(define (create-test-data count)
  (define (gen i)
    `(("_id" (s32 ,i))
      ("id" (s32 ,i))
      ("name" ,(string-append "name" (number->string i)))
      ("data" "some data")))
  (list->vector (do ((i 0 (+ i 1)) (r '() (cons (gen i) r)))
		    ((= i count) r))))

(when (get-environment-variable "MONGODB_RUNNING")
  (call-with-mongodb-database "localhost" 27017 "test"
   (lambda (db)
     (test-assert (mongodb-database? db))
     (mongodb-database-drop-collection db collection)
     (mongodb-database-insert db collection (create-test-data 1000))
     (let ((q (mongodb-database-query db collection '() 0 10)))
       (test-equal '("id" "id" "id" "id" "id" "id" "id" "id" "id" "id")
		   (mongodb-query-map (lambda (doc) (car (assoc "id" doc))) q))
       ;; a bit weird but legal
       (test-equal 1000 (mongodb-query-fold (lambda (doc r) (+ r 1)) 0 q #t))
       (test-error mongodb-invalid-cursor?
		   (mongodb-query-fold (lambda (doc r) (+ r 1)) 0 q #t))
       (test-equal 10
		   (mongodb-query-fold (lambda (doc r) (+ r 1)) 0 q)))
     (let ((q (mongodb-database-query db collection '() 0 10))
	   (r '())
	   (c 0))
       (test-equal '("id" "id" "id" "id" "id" "id" "id" "id" "id" "id")
		   (begin 
		     (mongodb-query-for-each
		      (lambda (doc) (set! r (cons (car (assoc "id" doc)) r))) q)
		     r))
       ;; a bit weird but legal
       (test-equal 1000
		   (begin 
		     (mongodb-query-for-each
		      (lambda (doc) (set! c (+ c 1))) q #t)
		     c))
       ))))
  
(test-end)
