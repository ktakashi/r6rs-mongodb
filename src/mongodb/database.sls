;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/database.sls - MongoDB database
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; The MongoDB database library represents a MongoDB database.
;; And it also contains database CRUD operations.

#!r6rs
(library (mongodb database)
    (export (rename (mongodb-base-database? mongodb-database?))
	    make-mongodb-database
	    mongodb-database-name
	    mongodb-database-connection
	    mongodb-invalid-cursor? mongodb-query-failure?
	    (rename (->full-collection-name mongodb-database->namespace))

	    mongodb-cursor? make-mongodb-cursor
	    mongodb-cursor-id
	    mongodb-cursor-ns
	    mongodb-cursor-database ;; internal only
	    
	    mongodb-query-result?
	    mongodb-query-result-id
	    mongodb-query-result-to
	    mongodb-query-result-starting-from
	    mongodb-query-result-documents

	    mongodb-database-get-last-error
	    mongodb-database-run-command
	    mongodb-database-drop-collection
	    mongodb-database-admin-command
	    
	    mongodb-database-query
	    mongodb-database-query/selector
	    mongodb-database-query-request ;; low level

	    mongodb-database-cursor-get-more
	    mongodb-database-kill-cursors
	    
	    mongodb-database-insert
	    mongodb-database-insert-request ;; low level
	    mongodb-database-insert-command

	    mongodb-database-update
	    mongodb-database-upsert
	    mongodb-database-update-all
	    mongodb-database-update-request ;; low level
	    mongodb-database-update-command

	    mongodb-database-delete
	    mongodb-database-delete-all
	    mongodb-database-delete-request ;; low level
	    mongodb-database-delete-command

	    ;; these are exposed so that we may implement
	    ;; async or queued call in the future
	    mongodb-database-receive-reply ;; low level
	    mongodb-database-send-query	   ;; low level
	    mongodb-database-send-get-more ;; low level

	    ;; for expansion
	    (rename (mongodb-base-database <mongodb-base-database>))
	    mongodb-database-default-message-sender
	    )
    (import (rnrs)
	    (mongodb connection)
	    (mongodb protocol)
	    (mongodb conditions)
	    (mongodb bson)
	    (mongodb util parameters))

(define-record-type mongodb-base-database
  (fields connection
	  name
	  message-sender
	  ;; frequently used
	  use-iso-date?)
  (protocol (lambda (p)
	      (lambda (connection name message-sender)
		(unless (mongodb-connection? connection)
		  (assertion-violation 'make-mongodb-database
				       "MongoDB connection is required"
				       connection))
		;; TODO should we check the format?
		;; (e.g. not containing '.')
		(unless (string? name)
		  (assertion-violation 'make-mongodb-database
				       "Database name must be a string"
				       name))
		(p connection name message-sender
		   (mongodb-connection-option-use-iso-date?
		    (mongodb-connection-option connection)))))))

(define mongodb-database-name mongodb-base-database-name)
(define mongodb-database-connection mongodb-base-database-connection)
(define mongodb-database-use-iso-date? mongodb-base-database-use-iso-date?)

(define-record-type mongodb-database
  (parent mongodb-base-database)
  (protocol (lambda (p)
	      (lambda (connection name)
		((p connection name
		    mongodb-database-default-message-sender))))))

(define-record-type mongodb-cursor
  (fields id				; cursor id
	  database			; database (for connection)
	  ns))				; namespace (full collection name)

;; NB AwaitCapable is always set so we don't handle it
(define-record-type mongodb-query-result
  (parent mongodb-cursor)
  (fields id		;; message id
	  to		;; response to
	  starting-from ;; for cursor
	  documents	;; documents
	  ))

(define-condition-type &mogodb-invalid-cursor &mongodb
  make-mongodb-invalid-cursor mongodb-invalid-cursor?)
(define-condition-type &mogodb-query-failure &mongodb
  make-mongodb-query-failure mongodb-query-failure?)
(define (raise-mongodb-error err who msg . irr)
  (raise (condition err
		    (make-who-condition who)
		    (make-message-condition msg)
		    (make-irritants-condition irr))))

;; We don't cache this since we can't detect changes of the
;; state of the connection.
(define (mongodb-database-output-port database)
  (mongodb-connection-output-port (mongodb-database-connection database)))
(define (mongodb-database-input-port database)
  (mongodb-connection-input-port (mongodb-database-connection database)))
(define (mongodb-database-request-id! database)
  (let ((c (mongodb-database-connection database)))
    ((mongodb-connection-request-id-strategy c) c database)))

(define (->full-collection-name database collection-names)
  (cond ((pair? collection-names)
	 (let-values (((out ext) (open-string-output-port)))
	   (put-string out (mongodb-database-name database))
	   (for-each (lambda (n) (put-char out #\.) (put-string out n))
		     collection-names)
	   (ext)))
	 ((or (not collection-names) (null? collection-names))
	  (mongodb-database-name database))
	 (else
	  (string-append (mongodb-database-name database)
			 "." collection-names))))

;; OP_QUERY
(define (op-reply->database-reply op-reply database fcn)
  (unless (op-reply? op-reply)
    (assertion-violation 'op-reply->database-reply "Unexpected object"
			 op-reply))
  (let ((response-flags (op-reply-response-flags op-reply)))
    (cond ((bitwise-bit-set? response-flags 0)
	   (raise-mongodb-error (make-mongodb-invalid-cursor)
				'op-reply->database-reply
				"Invalid cursor"))
	  ((bitwise-bit-set? response-flags 1)
	   (let ((err (vector-ref (op-reply-documents op-reply) 0)))
	     (raise-mongodb-error (make-mongodb-query-failure)
				  'op-reply->database-reply
				  "Failed to query"
				  err))))
    ;; we ignore the rest (i.e. 2 and 3)
    (make-mongodb-query-result
     (op-reply-cursor-id op-reply)
     database
     fcn
     (mongodb-protocol-message-request-id op-reply)
     (mongodb-protocol-message-response-to op-reply)
     (op-reply-starting-from op-reply)
     (op-reply-documents op-reply))))

;; Database authentication is a bit troublesome, so we don't do it here.
;; https://docs.mongodb.com/manual/core/authentication/
;; However, we provider enough APIs to implement this
;;
;; (define (mongodb-database-authenticate database username password)
;;   (let ((r (mongodb-database-run-command database
;; 	    `(("authenticate" 1) ("user" ,username) ("pwd" ,password)))))
;;     (display r) (newline)
;;     (cond ((assoc "ok" r) =>
;; 	   (lambda (slot) (not (zero? (cadr slot)))))
;; 	  (else #f))))

(define (mongodb-database-query database collection-names query . maybe-options)
  (define skipn (if (null? maybe-options) 0 (car maybe-options)))
  (define returnn (if (or (null? maybe-options) (null? (cdr maybe-options)))
		      0
		      (cadr maybe-options)))
  (mongodb-database-query-request
   database collection-names skipn returnn query #f))

(define (mongodb-database-query/selector database
					 collection-names
					 query
					 selectors
					 . maybe-options)
  (define skipn (if (null? maybe-options) 0 (car maybe-options)))
  (define returnn (if (or (null? maybe-options) (null? (cdr maybe-options)))
		      0
		      (cadr maybe-options)))
  (define rfs (map (lambda (s) (list s 1)) selectors))
  (mongodb-database-query-request
   database collection-names skipn returnn query rfs))

(define (mongodb-database-default-message-sender database msg)
  (define (get/generate-request-id database msg)
    (cond ((mongodb-protocol-message-request-id msg))
	  (else
	   (let ((request-id (mongodb-database-request-id! database)))
	     (mongodb-protocol-message-request-id-set! msg request-id)
	     request-id))))
  (let ((request-id (get/generate-request-id database msg)))
    (write-mongodb-message (mongodb-database-output-port database) msg)
    request-id))

(define (send-message database msg)
  ((mongodb-base-database-message-sender database) database msg))

(define (receive-reply database fcn)
  (parameterize ((*bson:use-iso-date?*
		  (mongodb-database-use-iso-date? database)))
    (op-reply->database-reply
     (read-mongodb-message (mongodb-database-input-port database))
     database fcn)))

(define (mongodb-database-receive-reply database)
  (check-connection-open 'mongodb-database-receive-reply database)
  ;; if this is called, then it should be async/queued request/response
  ;; style, so we don't care the book keeping atm.
  (receive-reply database #f))

(define (mongodb-database-query-request database col-names ns nr query rfs)
  (let ((fcn (->full-collection-name database col-names)))
    (mongodb-database-send-query database fcn ns nr query rfs)
    (receive-reply database fcn)))

(define (mongodb-database-send-query database fcn ns nr query rfs)
  (check-connection-open 'mongodb-database-query-request database)
  (let ((query (make-op-query fcn ns nr query rfs)))
    (send-message database query)))
  
;; returns #f or query-result
(define (mongodb-database-cursor-get-more cursor . opt)
  (define nr (if (null? opt) 0 (car opt)))
  (and (mongodb-database-send-get-more cursor nr)
       (let ((fcn (mongodb-cursor-ns cursor))
	     (database (mongodb-cursor-database cursor)))
	 (receive-reply database fcn))))

(define (mongodb-database-send-get-more query nr)
  (define cid (mongodb-cursor-id query))
  (define database (mongodb-cursor-database query))
  (check-connection-open 'mongodb-database-get-more database)
  (and (not (zero? cid))
       (let* ((fcn (mongodb-cursor-ns query))
	      (get-more (make-op-get-more fcn nr cid))
	      (to (mongodb-query-result-to query))
	      (out (mongodb-database-output-port database)))
	 (mongodb-protocol-message-request-id-set! get-more to)
	 (write-mongodb-message out get-more)
	 to)))

(define (mongodb-database-kill-cursors database . cursor*)
  (check-connection-open 'mongodb-database-kill-cursors database)
  (let ((kill (make-op-kill-cursors
	       (list->vector
		(filter positive?
			(map mongodb-cursor-id cursor*))))))
    (send-message database kill)))

;; OP_INSERT
(define (mongodb-database-insert database collection-names documents . options)
  ;; add ignore error flag
  (define flags (if (null? options) 0 (or (and (car options) 1) 0)))
  (mongodb-database-insert-request database collection-names flags documents))

(define (mongodb-database-insert-request db names flags doc*)
  (check-connection-open 'mongodb-database-insert-request db)
  (let ((insert (make-op-insert flags (->full-collection-name db names) doc*)))
    (send-message db insert)
    (unless (bitwise-bit-set? flags 0)
      (check-last-error 'mongodb-database-insert-request db))))

(define (->command-options opts)
  (if (null? opts)
      opts
      (map (lambda (p)
	     (if (symbol? (car p))
		 (cons (symbol->string (car p)) (cdr p))
		 p)) opts)))
;; insert command
(define (mongodb-database-insert-command database collection documents . opts)
  (mongodb-database-run-command database
   `(("insert" ,collection)
     ("documents" ,documents)
     . ,(->command-options opts))))

;; OP_UPDATE
(define (mongodb-database-update database collection-names selector update)
  (mongodb-database-update-request database collection-names 0 selector update))
(define (mongodb-database-upsert database collection-names selector update)
  (mongodb-database-update-request database collection-names 1 selector update))
(define (mongodb-database-update-all database collection-names selector update)
  (mongodb-database-update-request database collection-names 2 selector update))
(define (mongodb-database-update-request db names flags selector update)
  (check-connection-open 'mongodb-database-update-request db)
  (let ((update (make-op-update (->full-collection-name db names) flags
				selector update)))
    (send-message db update)
    (check-last-error 'mongodb-database-update-request db)))

;; update command
(define (mongodb-database-update-command database collection updates . opts)
  (mongodb-database-run-command database
   `(("update" ,collection)
     ("updates" ,updates)
     . ,(->command-options opts))))

;; OP_DELETE
(define (mongodb-database-delete db names selector)
  (mongodb-database-delete-request db names 1 selector))
(define (mongodb-database-delete-all db names selector)
  (mongodb-database-delete-request db names 0 selector))
(define (mongodb-database-delete-request db names flags selector)
  (check-connection-open 'mongodb-database-update-request db)
  (let ((delete (make-op-delete (->full-collection-name db names)
				flags selector)))
    (send-message db delete)
    (check-last-error 'mongodb-database-delete-request db)))

;; delete command
(define (mongodb-database-delete-command database collection deletes . opts)
  (mongodb-database-run-command database
   `(("delete" ,collection)
     ("deletes" ,deletes)
     . ,(->command-options opts))))

(define (mongodb-database-run-command db command)
  (define (ensure-bson command)
    (if (string? command)
	`((,command 1))
	command))
  (let ((r (mongodb-database-query-request db "$cmd" 1 1
					   (ensure-bson command) #f)))
    (vector-ref (mongodb-query-result-documents r) 0)))

(define (mongodb-database-get-last-error db)
  (mongodb-database-run-command db "getLastError"))

(define (mongodb-database-drop-collection db collection)
  ;; The documentation is incorrect... fxxk!!
  (mongodb-database-run-command db `(("drop" ,collection))))

(define (mongodb-database-admin-command db command)
  (mongodb-connection-run-command (mongodb-database-connection db) command))

(define (check-last-error who db)
  (let ((doc (mongodb-database-get-last-error db)))
    (cond ((assoc "err" doc) =>
	   (lambda (slot)
	     (unless (eq? 'null (cadr slot))
	       (raise-mongodb-error
		(make-mongodb-error) who (cadr slot) db)))))))

(define (check-connection-open who db)
  (unless (mongodb-connection-open? (mongodb-database-connection db))
    (raise-mongodb-error (make-mongodb-connection-closed)
			 who
			 "MongoDB connection must be opened"
			 db)))

)
