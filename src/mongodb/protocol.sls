;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/protocol.sls - Wire protocol
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
(library (mongodb protocol)
    (export read-mongodb-message
	    write-mongodb-message

	    msg-header? make-msg-header
	    msg-header-message-length
	    msg-header-request-id msg-header-request-id-set!
	    msg-header-response-to msg-header-response-to-set!
	    msg-header-op-code

	    mongodb-protocol-message?
	    mongodb-protocol-message-header

	    mongodb-query-message?
	    mongodb-query-message-full-collection-name

	    mongodb-flagged-query-message?
	    mongodb-flagged-query-message-flags

	    op-update? make-op-update
	    op-update-selector
	    op-update-update

	    op-insert? make-op-insert
	    op-insert-documents

	    op-query? make-op-query
	    op-query-number-to-skip
	    op-query-number-to-return
	    op-query-query
	    op-query-return-fields-selector

	    op-get-more? make-op-get-more
	    op-get-more-number-to-return
	    op-get-more-cursor-id

	    op-delete? make-op-delete
	    op-delete-selector

	    op-kill-cursors? make-op-kill-cursors
	    op-kill-cursors-number-of-cursor-ids
	    op-kill-cursors-cursor-ids

	    op-reply?
	    op-reply-response-flags
	    op-reply-cursor-id
	    op-reply-starting-from
	    op-reply-number-returned
	    op-reply-documents
	    )
    (import (rnrs)
	    (mongodb protocol msg-header)
	    (mongodb protocol op-update)
	    (mongodb protocol op-insert)
	    (mongodb protocol op-query)
	    (mongodb protocol op-get-more)
	    (mongodb protocol op-delete)
	    (mongodb protocol op-kill-cursors)
	    (mongodb protocol op-reply))

(define (read-mongodb-message in)
  (let* ((header (read-msg-header in))
	 (op-code (msg-header-op-code header)))
    (cond ((= op-code *op-code:reply*) (read-op-reply in header))
	  ((= op-code *op-code:update*) (read-op-update in header))
	  ((= op-code *op-code:insert*) (read-op-insert in header))
	  ((= op-code *op-code:query*) (read-op-query in header))
	  ((= op-code *op-code:get-more*) (read-op-get-more in header))
	  ((= op-code *op-code:delete*) (read-op-delete in header))
	  ((= op-code *op-code:kill-cursors*) (read-op-kill-cursors in header))
	  (else
	   (assertion-violation 'read-msg-header "Unknown OP code" op-code)))))


(define (write-mongodb-message out msg)
  (define header (mongodb-protocol-message-header msg))
  (let-values (((bout extract) (open-bytevector-output-port)))
    (cond ((op-update? msg) (write-op-update bout msg))
	  ((op-insert? msg) (write-op-insert bout msg))
	  ((op-query? msg) (write-op-query bout msg))
	  ((op-get-more? msg) (write-op-get-more bout msg))
	  ((op-delete? msg) (write-op-delete bout msg))
	  ((op-kill-cursors? msg) (write-op-kill-cursors bout msg))
	  ((op-reply? msg)
	   (assertion-violation 'write-mongodb-message
				"No reason to write OP_REPLY" msg))
	  (else
	   (assertion-violation 'write-mongodb-message
				"Unknown protocol message" msg)))
    (let ((bv (extract)))
      (msg-header-message-length-set! header (+ (bytevector-length bv)
						*msg-header-size*))
      (write-msg-header out header)
      (put-bytevector out bv)
      (flush-output-port out))))
)
