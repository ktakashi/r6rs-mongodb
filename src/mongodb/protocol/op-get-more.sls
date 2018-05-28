;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/protocol/op-get-more.sls - Wire protocol: OP_GET_MORE
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
(library (mongodb protocol op-get-more)
    (export op-get-more? make-op-get-more
	    op-get-more-number-to-return
	    op-get-more-cursor-id

	    read-op-get-more
	    read-op-get-more!
	    write-op-get-more
	    )
    (import (rnrs)
	    (mongodb util ports)
	    (mongodb protocol msg-header)
	    (mongodb bson parser)
	    (mongodb bson))

;; header
;; zero                 int32
;; full-collection-name cstring
;; number-to-return     int32
;; cursor-id            int64
(define-record-type op-get-more
  (parent mongodb-query-message)
  (fields zero				 ;; int32 (reserved)
	  (mutable number-to-return)	 ;; int32
	  (mutable cursor-id)		 ;; int64
	  )
  (protocol (lambda (p)
	      (define (check-header header)
		(unless (msg-header? header)
		  (assertion-violation 'make-op-get-more
				       "MsgHeader required" header))
		(unless (eqv? (msg-header-op-code header) *op-code:get-more*)
		  (assertion-violation 'make-op-get-more
				       "Invalid op-code" header)))
	      (case-lambda
	       (()
		(let ((header (make-msg-header)))
		  (msg-header-op-code-set! header *op-code:get-more*)
		  ((p header #f) 0 0 0)))
	       ((header zero fcn nr cid)
		(check-header header)
		((p header fcn) zero nr cid))))))

(define (read-op-get-more in header)
  (read-op-get-more! in (make-op-get-more header 0 #f 0 0)))

(define (read-op-get-more! in op-get-more)
  (define header (mongodb-protocol-message-header op-get-more))
  (let ((zero (get-s32 in)))
    (let-values (((fsize fcn) (get-cstring in)))
      (let* ((nr (get-s32 in))
	     (cid (get-s64 in)))
	(unless (eqv? (msg-header-content-length header) (+ fsize 16))
	  (assertion-violation 'read-op-get-more!
			       "Invalid size of message"
			       `(expected ,(msg-header-content-length header))
			       `(got ,(+ fsize 16))))
	(unless (zero? zero)
	  (assertion-violation 'read-op-get-more!
			       "Reserved value contains non 0" zero))
	(mongodb-query-message-full-collection-name-set! op-get-more fcn)
	(op-get-more-number-to-return-set! op-get-more nr)
	(op-get-more-cursor-id-set! op-get-more cid)
	op-get-more))))

;; this doesn't emit header
;; so must be called after header is written
(define (write-op-get-more out op-get-more)
  (put-s32 out 0) ;; zero
  (put-cstring out (mongodb-query-message-full-collection-name op-get-more))
  (put-s32 out (op-get-more-number-to-return op-get-more))
  (put-s64 out (op-get-more-cursor-id op-get-more)))

)
