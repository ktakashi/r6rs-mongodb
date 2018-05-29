;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/protocol/op-query.sls - Wire protocol: OP_QUERY
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
(library (mongodb protocol op-query)
    (export op-query? make-op-query
	    op-query-number-to-skip
	    op-query-number-to-return
	    op-query-query
	    op-query-return-fields-selector

	    read-op-query
	    read-op-query!
	    write-op-query
	    )
    (import (rnrs)
	    (mongodb util ports)
	    (mongodb protocol msg-header)
	    (mongodb bson parser)
	    (mongodb bson))

;; int32 flags 
;; cstring full-collection-name
(define-record-type op-query
  (parent mongodb-flagged-query-message)
  (fields (mutable number-to-skip) ;; int32
	  (mutable number-to-return) ;;int32
	  (mutable query)	     ;; document
	  (mutable return-fields-selector) ;; document?
	  )
  (protocol (lambda (p)
	      (define (check-header header)
		(unless (msg-header? header)
		  (assertion-violation 'make-op-query
				       "MsgHeader required" header))
		(unless (eqv? (msg-header-op-code header) *op-code:query*)
		  (assertion-violation 'make-op-query
				       "Invalid op-code" header)))
	      (case-lambda
	       ((fcn ns nr q rfs)
		(let ((header (make-msg-header)))
		  (msg-header-op-code-set! header *op-code:query*)
		  ((p header fcn 0) ns nr q rfs)))
	       ((header fl fcn ns nr q rfs)
		(check-header header)
		((p header fcn fl) ns nr q rfs))))))

(define (read-op-query in header)
  (read-op-query! in (make-op-query header 0 #f 0 0 '() #f)))

(define (read-op-query! in op-query)
  (define header (mongodb-protocol-message-header op-query))
  (define (finish op-query flags fcn ns nr q rfs)
    (mongodb-flagged-query-message-flags-set! op-query flags)
    (mongodb-query-message-full-collection-name-set! op-query fcn)
    (op-query-number-to-skip-set! op-query ns)
    (op-query-number-to-return-set! op-query nr)
    (op-query-query-set! op-query q)
    (op-query-return-fields-selector-set! op-query rfs)
    op-query)
  (let ((flags (get-u32 in)))
    (let-values (((fsize fcn) (get-cstring in)))
      (let* ((ns (get-s32 in)) ;; number-to-skip
	     (nr (get-s32 in))) ;; number-to-return
	(let-values (((qsize query) (read-document in)))
	  (if (= (msg-header-content-length header) (+ 12 fsize qsize))
	      (finish op-query flags fcn ns nr query #f)
	      (let-values (((rsize rfs) (read-document in)))
		(finish op-query flags fcn ns nr query rfs))))))))

;; this doesn't emit header
;; so must be called after header is written
(define (write-op-query out op-query)
  (put-u32 out (mongodb-flagged-query-message-flags op-query))
  (put-cstring out (mongodb-query-message-full-collection-name op-query))
  (put-s32 out (op-query-number-to-skip op-query))
  (put-s32 out (op-query-number-to-return op-query))
  (bson-write (op-query-query op-query) out)
  (let ((rfc (op-query-return-fields-selector op-query)))
    (when rfc (bson-write rfc out))))
	   
)
