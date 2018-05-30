;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/protocol/op-kill-cursors.sls - Wire protocol: OP_KILL_CURSORS
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
(library (mongodb protocol op-kill-cursors)
    (export op-kill-cursors? make-op-kill-cursors
	    op-kill-cursors-number-of-cursor-ids
	    op-kill-cursors-cursor-ids

	    read-op-kill-cursors
	    read-op-kill-cursors!
	    write-op-kill-cursors
	    )
    (import (rnrs)
	    (mongodb util ports)
	    (mongodb protocol msg-header)
	    (mongodb bson parser)
	    (mongodb bson))

(define-record-type op-kill-cursors
  (parent mongodb-protocol-message)
  (fields zero		 ;; int32
	  ;; (mutable number-of-cursor-ids) ;; int32
	  (mutable cursor-ids)		 ;; int64*
	  )
  (protocol (lambda (p)
	      (define (check-header header)
		(unless (msg-header? header)
		  (assertion-violation 'make-op-kill-cursors
				       "MsgHeader required" header))
		(unless (eqv? (msg-header-op-code header)
			      *op-code:kill-cursors*)
		  (assertion-violation 'make-op-kill-cursors
				       "Invalid op-code" header)))
	      (define (check-id* id*)
		(unless (vector? id*)
		  (assertion-violation 'make-op-kill-cursors
				       "cursor-ids must be a vector" id*)))
	      (case-lambda
	       ((id*)
		(check-id* id*)
		(let ((header (make-msg-header)))
		  (msg-header-op-code-set! header *op-code:kill-cursors*)
		  ((p header) 0 id*)))
	       ((header zero id*)
		(check-header header)
		(check-id* id*)
		((p header) zero id*))))))
(define (op-kill-cursors-number-of-cursor-ids op-kill-cursors)
  (vector-length (op-kill-cursors-cursor-ids op-kill-cursors)))

(define (read-op-kill-cursors in header)
  (read-op-kill-cursors! in (make-op-kill-cursors header 0 '#())))

(define (read-op-kill-cursors! in op-kill-cursors)
  (define header (mongodb-protocol-message-header op-kill-cursors))
  (define (finish op-kill-cursors zero n id*)
    (op-kill-cursors-cursor-ids-set! op-kill-cursors id*)
    op-kill-cursors)
  (let ((zero (get-u32 in)))
    (unless (zero? zero)
      (assertion-violation 'read-op-kill-cursors!
			   "Reserved value contains non 0" zero))
    (do ((n (get-u32 in))
	 (i 0 (+ i 1))
	 (id* '() (cons (get-s64 in) id*)))
	((= n i)
	 (finish op-kill-cursors zero n (list->vector (reverse id*)))))))

;; this doesn't emit header
;; so must be called after header is written
(define (write-op-kill-cursors out op-kill-cursors)
  (define id* (op-kill-cursors-cursor-ids op-kill-cursors))
  (put-s32 out 0)
  (put-u32 out (vector-length id*))
  (vector-for-each (lambda (id) (put-s64 out id)) id*))
	   
)
