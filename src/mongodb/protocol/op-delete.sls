;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/protocol/op-delete.sls - Wire protocol: OP_DELETE
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
(library (mongodb protocol op-delete)
    (export op-delete? make-op-delete
	    op-delete-selector

	    read-op-delete
	    read-op-delete!
	    write-op-delete
	    )
    (import (rnrs)
	    (mongodb util ports)
	    (mongodb protocol msg-header)
	    (mongodb bson parser)
	    (mongodb bson))

#|
struct {
    MsgHeader header;             // standard message header
    int32     ZERO;               // 0 - reserved for future use
    cstring   fullCollectionName; // "dbname.collectionname"
    int32     flags;              // bit vector - see below for details.
    document  selector;           // query object.  See below for details.
}
|#
(define-record-type op-delete
  (parent mongodb-flagged-query-message)
  (fields (mutable zero)		 ;; int32
	  (mutable selector)		 ;; document
	  )
  (protocol (lambda (p)
	      (define (check-header header)
		(unless (msg-header? header)
		  (assertion-violation 'make-op-delete
				       "MsgHeader required" header))
		(unless (eqv? (msg-header-op-code header) *op-code:delete*)
		  (assertion-violation 'make-op-delete
				       "Invalid op-code" header)))
	      (case-lambda
	       (()
		(let ((header (make-msg-header)))
		  (msg-header-op-code-set! header *op-code:delete*)
		  ((p header #f 0) 0 '())))
	       ((header zero fcn fl selector)
		(check-header header)
		((p header fcn fl) 0 selector))))))

(define (read-op-delete in header)
  (read-op-delete! in (make-op-delete header 0 #f 0 '())))

(define (read-op-delete! in op-delete)
  (define header (mongodb-protocol-message-header op-delete))
  (let ((zero (get-s32 in)))
    (let-values (((fsize fcn) (get-cstring in)))
      (let ((flags (get-u32 in)))
	(let-values (((dsize selector) (read-document in)))
	  (unless (eqv? (msg-header-content-length header) (+ fsize dsize 8))
	    (assertion-violation 'read-op-delete!
				 "Invalid size of message"
				 `(expected ,(msg-header-content-length header))
				 `(got ,(+ fsize dsize 8))))
	  (unless (zero? zero)
	    (assertion-violation 'read-op-delete!
				 "Reserved value contains non 0" zero))
	  (mongodb-query-message-full-collection-name-set! op-delete fcn)
	  (mongodb-flagged-query-message-flags-set! op-delete flags)
	  (op-delete-selector-set! op-delete selector)
	  op-delete)))))

;; this doesn't emit header
;; so must be called after header is written
(define (write-op-delete out op-delete)
  (put-s32 out 0)
  (put-cstring out (mongodb-query-message-full-collection-name op-delete))
  (put-u32 out (mongodb-flagged-query-message-flags op-delete))
  (bson-write (op-delete-selector op-delete) out))
	   
)
