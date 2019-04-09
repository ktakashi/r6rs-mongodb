;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/protocol/op-msg.sls - Wire protocol: OP_MSG
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

#!r6rs
(library (mongodb protocol op-msg)
    (export op-msg?
	    op-msg-flag-bits
	    op-msg-sections
	    op-msg-checksum

	    read-op-msg)
    (import (rnrs)
	    (mongodb util ports)
	    (mongodb protocol msg-header)
	    (mongodb bson parser))

;; The OP_MSG is immutable (it's server response)
(define-record-type op-msg
  (parent mongodb-protocol-message)
  (fields flag-bits	  ;; uint32
	  sections	  ;; Sections[]
	  checksum	  ;; uint32 (optional)
	  ))

(define (read-op-msg in header)
  (define (s size) (+ *msg-header-size* 4 size))
  (let ((flags (get-u32 in)))
    (let-values (((size sections) (read-sections in)))
      (let ((checksum (and (< (s size) (msg-header-message-length header))
			   (get-s32 in))))
	(make-op-msg header flags sections checksum)))))

(define (read-sections in)
  (let ((kind (get-u8 in)))
    (case kind
      ((0) (let-values (((size doc) (read-document in)))
	     (values (+ size 1) doc)))
      (else (assertion-violation 'read-op-msg "Unknown kind" kind)))))
	   
)
