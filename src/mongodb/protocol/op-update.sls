;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/protocol/op-update.sls - Wire protocol: MsgHeader
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
(library (mongodb protocol op-update)
    (export op-update? make-op-update
	    op-update-full-collection-name
	    op-update-flags
	    op-update-selector
	    op-update-update

	    read-op-update
	    read-op-update!
	    )
    (import (rnrs)
	    (mongodb util ports)
	    (mongodb protocol msg-header)
	    (mongodb bson parser))

(define-record-type op-update
  (parent mongodb-protocol-message)
  (fields zero				 ;; int32 (reserved)
	  (mutable full-collection-name) ;; cstring
	  (mutable flags)		 ;; int32
	  (mutable selector)		 ;; document
	  (mutable update)		 ;; document
	  )
  (protocol (lambda (p)
	      (define (check-header header)
		(unless (msg-header? header)
		  (assertion-violation 'make-op-update
				       "MsgHeader required" header)))
	      (case-lambda
	       ((header)
		(check-header header)
		((p header) 0 #f #f #f #f))
	       ((header zero fcn fl s u)
		(check-header header)
		((p header) zero fcn fl s u))))))

(define (read-op-update in header)
  (read-op-update! in (make-op-update header)))

(define (read-op-update! in op-update)
  (define header (mongodb-protocol-message-header op-update))
  (let ((zero (get-s32 in)))
    (let-values (((fsize fcn) (get-cstring in)))
	 ;; even thought it says int32 we use u32 (for future
	 ;; when the most significant bit is set)
      (let ((flags (get-u32 in)))
	(let*-values (((ssize selector) (read-document in))
		      ((usize update) (read-document in)))
	  (unless (= (msg-header-message-length header)
		     (+ ssize usize fsize 8 *msg-header-size*))
	    (assertion-violation 'read-op-update!
	     "Invalid size of message"
	     `(expected ,(msg-header-message-length header))
	     `(got ,(+ ssize usize fsize 8 *msg-header-size*))))
	  (unless (zero? zero)
	    (assertion-violation 'read-op-update!
				 "Reserved value contains non 0" zero))
	  (op-update-full-collection-name-set! op-update fcn)
	  (op-update-flags-set! op-update flags)
	  (op-update-selector-set! op-update selector)
	  (op-update-update-set! op-update update)
	  op-update)))))
)
