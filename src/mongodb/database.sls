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
    (export mongodb-database? make-mongodb-database
	    mongodb-invalid-cursor? mongodb-query-failure?

	    mongodb-query-result?
	    mongodb-query-result-id
	    mongodb-query-result-to
	    mongodb-query-result-cursor-id
	    mongodb-query-result-starting-from
	    mongodb-query-result-documents

	    mongodb-database-query
	    mongodb-database-query/selector
	    mongodb-request-query ;; low level
	    )
    (import (rnrs)
	    (mongodb connection)
	    (mongodb protocol)
	    (mongodb conditions))

(define-record-type mongodb-database
  (fields connection
	  name
	  ;; I'm not entirely sure if this should be here or not,
	  ;; but we can't use SRFI-18 so global location isn't an
	  ;; option either...
	  (mutable request-id))
  (protocol (lambda (p)
	      (lambda (connection name)
		(unless (mongodb-connection? connection)
		  (assertion-violation 'make-mongodb-database
				       "MongoDB connection is required"
				       connection))
		(unless (mongodb-connection-open? connection)
		  (assertion-violation 'make-mongodb-database
				       "MongoDB connection must be opened"
				       connection))
		;; TODO should we check the format?
		;; (e.g. not containing '.')
		(unless (string? name)
		  (assertion-violation 'make-mongodb-database
				       "Database name must be a string"
				       name))
		(p connection name 1)))))

;; NB AwaitCapable is always set so we don't handle it
(define-record-type mongodb-query-result
  (fields id		;; message id
	  to		;; response to
	  cursor-id	;; for next query
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
  (let ((id (mongodb-database-request-id database)))
    (mongodb-database-request-id-set! database (+ id 1))
    id))
  
(define (->full-collection-name database collection-names)
  (if (pair? collection-names)
      (let-values (((out ext) (open-string-output-port)))
	(put-string out (mongodb-database-name database))
	(for-each (lambda (n) (put-char out #\.) (put-string out n))
		  collection-names)
	(ext))
      (string-append (mongodb-database-name database) "." collection-names)))

(define (op-reply->database-reply op-reply)
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
     (mongodb-protocol-message-request-id op-reply)
     (mongodb-protocol-message-response-to op-reply)
     (op-reply-cursor-id op-reply)
     (op-reply-starting-from op-reply)
     (op-reply-documents op-reply))))

(define (mongodb-database-query database collection-names query . maybe-options)
  (define skipn (if (null? maybe-options) 0 (car maybe-options)))
  (define returnn (if (or (null? maybe-options) (null? (cdr maybe-options)))
		      0
		      (cadr maybe-options)))
  (mongodb-request-query database collection-names skipn returnn query #f))

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
  (mongodb-request-query database collection-names skipn returnn query rfs))

(define (mongodb-request-query database col-names ns nr query rfs)
  (let ((query (make-op-query (->full-collection-name database col-names)
			      ns nr query rfs)))
    (mongodb-protocol-message-request-id-set! query
     (mongodb-database-request-id! database))
    (write-mongodb-message (mongodb-database-output-port database) query)
    (op-reply->database-reply
     (read-mongodb-message (mongodb-database-input-port database)))))
)
