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
	    (mongodb bson)
	    (mongodb bson parser))

;; The OP_MSG is immutable (it's server response)
(define-record-type op-msg
  (parent mongodb-protocol-message)
  (fields flag-bits	  ;; uint32
	  sections	  ;; Sections[]
	  checksum	  ;; uint32 (optional)
	  ))

(define (read-op-msg in header)
  (define (s offset)
    (- (msg-header-message-length header) (+ *msg-header-size* 4 offset)))
  (let* ((flags (get-u32 in))
	 (checksum? (bitwise-bit-set? flags 0))
	 (sections (read-sections in (s (if checksum? 4 0))))
	 (checksum (and checksum? (get-u32 in))))
    (make-op-msg header flags sections checksum)))

(define (read-sections oin content-size)
  (define in (open-bytevector-input-port (get-bytevector-n oin content-size)))
  (let loop ((r '()))
    (if (eof-object? (lookahead-u8 in))
	(list->vector (reverse r))
	(let ((kind (get-u8 in)))
	  (case kind
	    ((0) (loop (cons (bson-read in) r)))
	    ((1) (let* ((size (get-u32 in))
			(bv (get-bytevector-n in (- size 4)))
			(bin (open-bytevector-input-port bv))
			(seq-id (read-cstring bin)))
		   (let loop2 ((doc '()))
		     (if (eof-object? (lookahead-u8 bin))
			 (loop  (cons (cons seq-id (reverse doc)) r))
			 (loop2 (cons (bson-read bin) doc))))))
	    (else (assertion-violation 'read-op-msg "Unknown kind" kind)))))))
	   
)
