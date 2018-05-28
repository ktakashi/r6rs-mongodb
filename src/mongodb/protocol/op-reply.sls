;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/protocol/op-reply.sls - Wire protocol: OP_INSERT
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
(library (mongodb protocol op-reply)
    (export op-reply?
	    op-reply-response-flags
	    op-reply-cursor-id
	    op-reply-starting-from
	    op-reply-number-returned
	    op-reply-documents

	    read-op-reply)
    (import (rnrs)
	    (mongodb util ports)
	    (mongodb protocol msg-header)
	    (mongodb bson parser)
	    (mongodb bson))

;; The OP_REPLY is immutable (it's server response)
(define-record-type op-reply
  (parent mongodb-protocol-message)
  (fields response-flags		 ;; int32
	  cursor-id			 ;; int64
	  starting-from			 ;; int32
	  number-returned		 ;; int32
	  documents			 ;; documents*
	  ))

(define (read-op-reply in header)
  (let* ((flags (get-u32 in))
	 (cursor-id (get-s64 in))
	 (from (get-s32 in))
	 (nr (get-s32 in)))
    (do ((i 0 (+ i 1)) (doc '() (cons (bson-read in) doc)))
	((= nr i) (make-op-reply header flags cursor-id from nr
				 (list->vector (reverse doc)))))))

	   
)
