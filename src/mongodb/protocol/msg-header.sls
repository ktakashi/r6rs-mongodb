;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/protocol/msg-header.sls - Wire protocol: MsgHeader
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
(library (mongodb protocol msg-header)
    (export msg-header? make-msg-header
	    msg-header-message-length msg-header-message-length-set!
	    msg-header-request-id msg-header-request-id-set!
	    msg-header-response-to msg-header-response-to-set!
	    msg-header-op-code msg-header-op-code-set!
	    msg-header-content-length

	    read-msg-header
	    read-msg-header!

	    write-msg-header

	    mongodb-protocol-message mongodb-protocol-message?
	    mongodb-protocol-message-header

	    mongodb-query-message mongodb-query-message?
	    mongodb-query-message-full-collection-name
	    mongodb-query-message-full-collection-name-set!

	    mongodb-flagged-query-message mongodb-flagged-query-message?
	    mongodb-flagged-query-message-flags
	    mongodb-flagged-query-message-flags-set!
	    
	    *msg-header-size*
	    *op-code:reply*
	    *op-code:update*
	    *op-code:insert*
	    *op-code:query*
	    *op-code:get-more*
	    *op-code:delete*
	    *op-code:kill-cursors*
	    *op-code:msg*
	    )
    (import (rnrs)
	    (mongodb util ports))

(define *msg-header-size* 16)
(define *op-code:reply*        1)
(define *op-code:update*       2001)
(define *op-code:insert*       2002)
(define *op-code:query*        2004)
(define *op-code:get-more*     2005)
(define *op-code:delete*       2006)
(define *op-code:kill-cursors* 2007)
(define *op-code:msg*          2013)

(define-record-type msg-header
  (fields (mutable message-length) ;; int32
	  (mutable request-id)     ;; int32
	  (mutable response-to)    ;; int32
	  (mutable op-code))       ;; int32
  (protocol (lambda (p)
	      (case-lambda
	       (() (p #f #f #f #f))
	       ((len id to op) (p len id to op))))))
(define (msg-header-content-length header)
  (- (msg-header-message-length header) *msg-header-size*))

(define (read-msg-header in)
  (read-msg-header! in (make-msg-header)))

(define (read-msg-header! in msg)
  (let* ((len (get-s32 in))
	 (id (get-s32 in))
	 (to (get-s32 in))
	 (op (get-s32 in)))
    (msg-header-message-length-set! msg len)
    (msg-header-request-id-set! msg id)
    (msg-header-response-to-set! msg to)
    (msg-header-op-code-set! msg op)
    msg))


(define (write-msg-header out header)
  (put-s32 out (msg-header-message-length header))
  (put-s32 out (msg-header-request-id header))
  (put-s32 out (msg-header-response-to header))
  (put-s32 out (msg-header-op-code header)))

(define-record-type mongodb-protocol-message
  (fields header)) ;; MsgHeader

(define-record-type mongodb-query-message
  (parent mongodb-protocol-message)
  (fields (mutable full-collection-name))) ;; cstring

(define-record-type mongodb-flagged-query-message
  (parent mongodb-query-message)
  (fields (mutable flags)))
)
