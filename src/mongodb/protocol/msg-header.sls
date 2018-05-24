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
	    msg-header-message-length
	    msg-header-request-id
	    msg-header-response-to
	    msg-header-op-code

	    read-msg-header
	    read-msg-header!

	    mongodb-protocol-message mongodb-protocol-message?
	    mongodb-protocol-message-header mongodb-protocol-message-header-set!

	    *msg-header-size*
	    )
    (import (rnrs)
	    (mongodb util ports))

(define *msg-header-size* 16)

(define-record-type msg-header
  (fields (mutable message-length) ;; int32
	  (mutable request-id)     ;; int32
	  (mutable response-to)    ;; int32
	  (mutable op-code))       ;; int32
  (protocol (lambda (p)
	      (case-lambda
	       (() (p #f #f #f #f))
	       ((len id to op) (p len id to op))))))

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

(define-record-type mongodb-protocol-message
  (fields (mutable header)))

)
