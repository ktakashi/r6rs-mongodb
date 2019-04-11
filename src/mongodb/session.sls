;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/session.sls - MongoDB database
;;;
;;;   Copyright (c) 2019  Takashi Kato  <ktakashi@ymail.com>
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

;; The MongoDB session library represents a MongoDB session.
;; This contains transaction related procedures which only works
;; on replica set configured environment
#!r6rs
(library (mongodb session)
    (export (rename (make-mongodb-session mongodb-session-start))
	    mongodb-session?
	    mongodb-session-id
	    mongodb-session-end!
	    mongodb-session-start-transaction!
	    mongodb-session-commit-transaction!
	    mongodb-session-abort-transaction!

	    (rename (make-mongodb-session-database mongodb-session-database))
	    )
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (mongodb connection)
	    (mongodb database)
	    (mongodb protocol)
	    (mongodb util uuid))

(define-record-type mongodb-session
  (fields connection
	  (mutable transaction-number)
	  (mutable transaction-started?)
	  (mutable statement-ids)
	  id)
  (protocol (lambda (p)
	      (lambda (connection)
		(unless (mongodb-connection? connection)
		  (assertion-violation 'make-mongodb-session
				       "MongoDB connection is required"
				       connection))
		(p connection #f #f '() (make-v4-uuid))))))

(define-record-type mongodb-session-database
  (parent <mongodb-base-database>)
  (fields session)
  (protocol (lambda (p)
	      (lambda (session name)
		(unless (mongodb-session? session)
		  (assertion-violation 'make-mongodb-session-database
				       "MongoDB session is required" session))
		((p (mongodb-session-connection session) name 
		    mongodb-session-message-sender)
		 session)))))

(define (mongodb-session-message-sender database msg)
  ;; TODO also OP_MSG?
  (when (op-query? msg)
    (query-add-session-info (mongodb-session-database-session database) msg))
  (mongodb-database-default-message-sender database msg))

(define (mongodb-session-end! session)
  (define id (mongodb-session-id session))
  (mongodb-connection-run-command (mongodb-session-connection session)
				  `(("endSessions" #((("id" (uuid ,id))))))))
				    
(define (mongodb-session-start-transaction! session)
  (define tx (mongodb-session-transaction-number session))
  (define ids (mongodb-session-statement-ids session))

  (mongodb-session-transaction-started?-set! session #f)
  (mongodb-session-statement-ids-set! session (cons 0 ids))
  (mongodb-session-transaction-number-set! session (or (and tx (+ tx 1)) 0)))

(define (mongodb-session-commit-transaction! session)
  (mongodb-session-send-transaction-command session "commitTransaction"))
(define (mongodb-session-abort-transaction! session)
  (mongodb-session-send-transaction-command session "abortTransaction"))

;; private
(define (query-add-session-info session msg)
  (define query (op-query-query msg))
  (define lsid (mongodb-session-id session))
  (define tx   (mongodb-session-transaction-number session))
  (define ts? (mongodb-session-transaction-started? session))
  (define ids (mongodb-session-statement-ids session))
  
  (mongodb-session-transaction-started?-set! session #t)
  (op-query-query-set! msg `(,@query
			     ("lsid" (("id" (uuid ,lsid))))
			     ,@(if tx
				   `(("txnNumber" (s64 ,tx))
				     ("autocommit" #f)
				     ,@(if (not ts?)
					   '(("startTransaction" #t))
					   '())
				     ("stmtId" (s32 ,(update-ids! ids))))
				   '()))))

(define (mongodb-session-send-transaction-command session command)
  (define id (mongodb-session-id session))
  (define tx (mongodb-session-transaction-number session))
  (define ids (mongodb-session-statement-ids session))
  (unless tx
    (assertion-violation 'mongodb-session-commit-transaction! "No transaction"))
  (mongodb-session-transaction-number-set! session (and (> tx 0) (- tx 1)))
  (mongodb-connection-run-command
   (mongodb-session-connection session)
   `((,command 1)
     ("lsid" (("id" (uuid ,id))))
     ("autocommit" #f)
     ("txnNumber" (s64 ,tx))
     ("stmtId" (s32 ,(update-ids! ids))))))

(define (update-ids! ids)
  (let ((r (car ids)))
    (set-car! ids (+ r 1))
    r))
)
