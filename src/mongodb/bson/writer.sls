;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/bson/writer.sls - BSON writer
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
(library (mongodb bson writer)
    (export write-document
	    write-element
	    write-min-key-element
	    write-max-key-element
	    write-double-element
	    
	    write-cstring
	    write-double)
    (import (rnrs)
	    (mongodb bson conditions))

(define (write-document out document)
  (let-values (((bout extract) (open-bytevector-output-port)))
    (put-bytevector bout #vu8(0 0 0 0)) ;; put dummy size
    (for-each (lambda (e) (write-element bout e)) document)
    (let ((bv (extract)))
      (bytevector-s32-set! bv 0 (bytevector-length bv) (endianness little))
      (put-bytevector out bv))))

(define (write-element out element)
  (define (write-by-type out element)
    (cond ((find (lambda (writer) ((car writer) (cadr element))) *writers*) =>
	   (lambda (writer) ((cdr writer) out element)))
	  (else (raise-bson-write-error 'bson-write "Unknown element" element))))
  (unless (and (pair? element)
	       (string? (car element)) (not (null? (cdr element))))
    (raise-bson-write-error 'bson-write "Invalid element" element))
  (write-by-type out element))

(define (write-min-key-element out element)
  (put-u8 out #xFF)
  (write-cstring out (car element)))
(define (write-max-key-element out element)
  (put-u8 out #x7F)
  (write-cstring out (car element)))

(define (write-double-element out element)
  (put-u8 out #x01)
  (write-cstring out (car element))
  (write-double out (cadr element)))

(define (type-of? name) (lambda (v) (and (pair? v) (eq? name (car v)))))
(define (symbol-of? name) (lambda (v) (eq? name v)))
(define *writers*
  `((,(symbol-of? 'min-key) ,write-min-key-element)
    (,(symbol-of? 'max-key) ,write-max-key-element)
    ;; other numbers are typed.
    ,number? ,write-double-element
    ))

(define (write-cstring out cstring)
  (let ((bv (string->utf8 cstring)))
    (do ((len (bytevector-length bv)) (i 0 (+ i 1)))
	((= i len) (put-bytevector out bv) (put-u8 out 0))
      (when (zero? (bytevector-u8-ref bv i))
	(raise-bson-write-error 'bson-write
				"cstring mustn't contain 0" cstring)))))

(define (write-double out double)
  ;; FIXME a bit inefficient
  (let ((bv (make-bytevector 8)))
    (bytevector-ieee-double-set! bv 0 double (endianness little))
    (put-bytevector out bv)))

(define (raise-bson-write-error who msg . irr)
  (raise (condition
	  (make-bson-error)
	  (make-i/o-read-error)
	  (make-who-condition who)
	  (make-message-condition msg)
	  (make-irritants-condition irr))))
)

