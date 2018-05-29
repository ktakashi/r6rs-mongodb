;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/protocol/op-insert.sls - Wire protocol: OP_INSERT
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
(library (mongodb protocol op-insert)
    (export op-insert? make-op-insert
	    op-insert-documents

	    read-op-insert
	    read-op-insert!
	    write-op-insert
	    )
    (import (rnrs)
	    (mongodb util ports)
	    (mongodb protocol msg-header)
	    (mongodb bson parser)
	    (mongodb bson))

(define-record-type op-insert
  (parent mongodb-flagged-query-message)
  (fields ;;(mutable flags)		 ;; int32
	  ;;(mutable full-collection-name) ;; cstring
	  (mutable documents)		 ;; document*
	  )
  (protocol (lambda (p)
	      (define (check-header header)
		(unless (msg-header? header)
		  (assertion-violation 'make-op-insert
				       "MsgHeader required" header))
		(unless (eqv? (msg-header-op-code header) *op-code:insert*)
		  (assertion-violation 'make-op-insert
				       "Invalid op-code" header)))
	      (define (check-documents doc*)
		(unless (vector? doc*)
		  (assertion-violation 'make-op-insert
				       "documents must be a vector" doc*)))
	      (case-lambda
	       ((fl fcn doc*)
		(check-documents doc*)
		(let ((header (make-msg-header)))
		  (msg-header-op-code-set! header *op-code:insert*)
		  ((p header fcn fl) doc*)))
	       ((header fl fcn doc*)
		(check-header header)
		(check-documents doc*)
		((p header fcn fl) doc*))))))

(define (read-op-insert in header)
  (read-op-insert! in (make-op-insert header 0 #f '#())))

(define (read-op-insert! in op-insert)
  (define header (mongodb-protocol-message-header op-insert))
  (define (finish op-insert flags fcn doc*)
    (mongodb-flagged-query-message-flags-set! op-insert flags)
    (mongodb-query-message-full-collection-name-set! op-insert fcn)
    (op-insert-documents-set! op-insert doc*)
    op-insert)
  (let ((flags (get-u32 in)))
    (let-values (((fsize fcn) (get-cstring in)))
      (define rest (- (msg-header-content-length header) fsize 4))
      ;; we need to trace the size of the message here
      (let loop ((rest rest) (doc* '()))
	(if (zero? rest)
	    (finish op-insert flags fcn (list->vector (reverse doc*)))
	    (let-values (((dsize doc) (read-document in)))
	      (loop (- rest dsize) (cons doc doc*))))))))

;; this doesn't emit header
;; so must be called after header is written
(define (write-op-insert out op-insert)
  (put-u32 out (mongodb-flagged-query-message-flags op-insert))
  (put-cstring out (mongodb-query-message-full-collection-name op-insert))
  (vector-for-each (lambda (doc) (bson-write doc out))
		   (op-insert-documents op-insert)))
	   
)
