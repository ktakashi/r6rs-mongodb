;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/util/ports.sls - Port utilities
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
(library (mongodb util ports)
    (export get-u32
	    get-s32
	    get-u64
	    get-s64
	    get-f64
	    get-n-bytevector

	    put-u32
	    put-s32
	    put-u64
	    put-s64
	    put-f64

	    ;; these are common in wire protocol
	    get-cstring
	    get-string
	    )
    (import (rnrs))

;; in MongoDB all integers are little endian
(define (get-u32 in)
  (let ((bv (get-n-bytevector in 4)))
    (bytevector-u32-ref bv 0 (endianness little))))
(define (get-s32 in)
  (let ((bv (get-n-bytevector in 4)))
    (bytevector-s32-ref bv 0 (endianness little))))
(define (get-f64 in)
  (let ((bv (get-n-bytevector in 8)))
    (bytevector-ieee-double-ref bv 0 (endianness little))))
(define (get-s64 in)
  (let ((bv (get-n-bytevector in 8)))
    (bytevector-s64-ref bv 0 (endianness little))))
(define (get-u64 in)
  (let ((bv (get-n-bytevector in 8)))
    (bytevector-u64-ref bv 0 (endianness little))))

(define-syntax define-put
  (syntax-rules ()
    ((_ name size proc)
     (define (name out v)
       (define bv (make-bytevector size))
       (proc bv 0 v (endianness little))
       (put-bytevector out bv)))))
(define-put put-u32 4 bytevector-u32-set!)
(define-put put-s32 4 bytevector-s32-set!)
(define-put put-u64 8 bytevector-u64-set!)
(define-put put-s64 8 bytevector-s64-set!)
(define-put put-f64 8 bytevector-ieee-double-set!)

;; return 2 values 
(define (get-cstring in)
  (let-values (((out extract) (open-bytevector-output-port)))
    (let loop ((count 0))
      (let ((u8 (get-u8 in)))
	(cond ((eof-object? u8)
	       (raise-read-error 'get-cstring "Unexpected EOF"))
	      ((zero? u8) (values (+ count 1) (utf8->string (extract))))
	      (else (put-u8 out u8) (loop (+ count 1))))))))

;; return 2 values 
(define (get-string in)
  (let* ((size (get-s32 in))
	 (s    (get-n-bytevector in (- size 1)))
	 (end-mark (get-u8 in)))
    (unless (eqv? end-mark 0)
      (raise-read-error 'get-cstring "String must be followed by 0"
			end-mark))
    (values (+ size 4) (utf8->string s))))


;; make sure we read required length of bytes in case of socket port
(define (get-n-bytevector in n)
  (define bv (make-bytevector n))
  (let loop ((r n) (i 0))
    (let ((c (get-bytevector-n! in bv i r)))
      (cond ((eof-object? c)
	     (raise-read-error 'get-n-bytevector "Unexpected EOF"))
	    ((= c r) bv)
	    (else (loop (- r c) (+ i c)))))))

(define (raise-read-error who msg . irr)
  (raise (condition (make-i/o-read-error)
		    (make-who-condition who)
		    (make-message-condition msg)
		    (make-irritants-condition irr))))
    
)
