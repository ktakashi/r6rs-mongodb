;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb.sls - MongoDB binding
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

#!r6rs
(library (mongodb)
    (export mongodb-connection? make-mongodb-connection
	    open-mongodb-connection!
	    close-mongodb-connection!
	    mongodb-connection-open?
	    mongodb-connection-list-databases

	    mongodb-connection-option?
	    make-mongodb-connection-option

	    ;; for convenience
	    call-with-mongodb-connection
	    call-with-mongodb-database
	    
	    ;; database and crud operations
	    mongodb-database? make-mongodb-database
	    mongodb-database-query
	    mongodb-database-query/selector
	    mongodb-database-insert
	    mongodb-database-insert-request
	    mongodb-database-update
	    mongodb-database-upsert
	    mongodb-database-update-all
	    mongodb-database-delete
	    mongodb-database-delete-all
	    mongodb-database-get-last-error
	    mongodb-database-drop-collection

	    ;; cursor
	    mongodb-cursor? make-mongodb-cursor
	    mongodb-cursor-id mongodb-cursor-ns
	    
	    ;; query
	    mongodb-query-result?
	    mongodb-query-result-id
	    mongodb-query-result-to
	    mongodb-query-result-starting-from
	    mongodb-query-result-documents
	    
	    mongodb-query-result->generator
	    mongodb-query-result->get-more-generator
	    mongodb-query-fold
	    mongodb-query-for-each
	    mongodb-query-map
	    
	    ;; condition predicates
	    bson-error? ;; for convenience
	    mongodb-error?
	    mongodb-connection-error?
	    mongodb-connection-closed?
	    mongodb-invalid-cursor?
	    mongodb-query-failure?
	    
	    ;; low level APIs
	    mongodb-connection-run-command
	    
	    mongodb-database-run-command
	    mongodb-database-admin-command
	    mongodb-database-query-request

	    mongodb-database-cursor-get-more
	    mongodb-database-kill-cursors
	    
	    mongodb-database-insert-command

	    mongodb-database-update-request
	    mongodb-database-update-command

	    mongodb-database-delete-request
	    mongodb-database-delete-command)
    (import (rnrs)
	    (mongodb conditions)
	    (mongodb connection)
	    (mongodb database)
	    (mongodb bson))

;; SRFI-128/SRFI-158 style generator
(define (mongodb-query-result->generator query . maybe-kill-cursor?)
  (define index 0)
  (define documents (mongodb-query-result-documents query))
  (define document-size (vector-length documents))
  (define killed? (if (null? maybe-kill-cursor?)
		      #t
		      (not (car maybe-kill-cursor?))))
  (lambda ()
    (if (= index document-size)
	(begin
	  (unless killed?
	    (unless (zero? (mongodb-cursor-id query))
	      (mongodb-database-kill-cursors
	       (mongodb-cursor-database query) query)))
	  (eof-object))
	(let ((r (vector-ref documents index)))
	  (set! index (+ index 1))
	  r))))

(define (mongodb-query-result->get-more-generator query)
  (define index 0)
  (define current-query query)
  (define documents (mongodb-query-result-documents current-query))
  (define document-size (vector-length documents))

  (define (try-retrieve!)
    (let ((next (mongodb-database-cursor-get-more current-query)))
      (set! index 0)
      (set! current-query next)
      (when current-query
	(set! documents (mongodb-query-result-documents current-query))
	(set! document-size (vector-length documents)))))
  (lambda ()
    (let loop ()
      (cond ((not current-query) (eof-object))
	    ((= index document-size) (try-retrieve!) (loop))
	    (else
	     (let ((r (vector-ref documents index)))
	       (set! index (+ index 1))
	       r))))))

(define (mongodb-query-fold proc seed query . maybe-all?)
  (define ->generator
    (if (or (null? maybe-all?) (not (car maybe-all?)))
	mongodb-query-result->generator
	mongodb-query-result->get-more-generator))
  (let ((g (->generator query)))
    (do ((e (g) (g)) (r seed (proc e r)))
	((eof-object? e) r))))

(define (mongodb-query-for-each proc query . opt)
  (apply mongodb-query-fold (lambda (e r) (proc e)) #f query opt))
(define (mongodb-query-map proc query . opt)
  (reverse 
   (apply mongodb-query-fold (lambda (e r) (cons (proc e) r)) '() query opt)))

(define (call-with-mongodb-connection host port proc . maybe-option)
  (let ((conn (open-mongodb-connection!
	       (apply make-mongodb-connection host port maybe-option))))
    (guard (e (else (close-mongodb-connection! conn) (raise e)))
      (let ((r (proc conn)))
	(close-mongodb-connection! conn)
	r))))

(define (call-with-mongodb-database host port database proc . maybe-option)
  (apply call-with-mongodb-connection host port
	 (lambda (conn)
	   (proc (make-mongodb-database conn database)))
	 maybe-option))

)
