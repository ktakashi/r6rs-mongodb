;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/util/uuid.sls - UUID utilities
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

;; simple UUID utilities, it converts bytector to string or vice versa
#!r6rs
(library (mongodb util uuid)
    (export bytevector->uuid-string
	    uuid-string->bytevector
	    make-v4-uuid)
    (import (rnrs)
	    (mongodb util random)
	    (mongodb util bytevectors))

(define (bytevector->uuid-string bv)
  ;; should we?
  (define (bytevector-u40-ref bv index)
    (let ((u16 (bytevector-u16-ref bv index (endianness big)))
	  (u32 (bytevector-u32-ref bv (+ index 2) (endianness big))))
      (bitwise-ior (bitwise-arithmetic-shift-left u16 32) u32)))
  (let ((low (bytevector-u32-ref bv 0 (endianness big)))
	(mid (bytevector-u16-ref bv 4 (endianness big)))
	(high (bytevector-u16-ref bv 6 (endianness big)))
	(clock-var (bytevector-u8-ref bv 8))
	(clock-low (bytevector-u8-ref bv 9))
	(node (bytevector-u40-ref bv 10)))
    (make-uuid low mid high clock-var clock-low node)))

;; TODO implement efficiently...
(define (uuid-string->bytevector s)
  (unless (= (string-length s) 36)
    (assertion-violation 'uuid-string->bytevector "Invalid length of UUID" s))
  (let ((in (open-string-input-port s)))
    (let-values (((out e) (open-string-output-port)))
      (let loop ()
	(let ((c (get-char in)))
	  (if (eof-object? c)
	      (hex-string->bytevector (e))
	      (case c
		((#\-) (loop))
		(else (put-char out c) (loop)))))))))

(define (make-v4-uuid)
  (define low (random-integer #xffffffff))
  (define mid (random-integer #xffff))
  (define high (bitwise-ior #x4000
			    (bitwise-and #x0FFF (random-integer #xffff))))
  (define clock-var (bitwise-ior #x80 (bitwise-and #x3F (random-integer #xff))))
  (define clock-low (random-integer #xff))
  (define node (random-integer #xffffffffffff))
  (make-uuid low mid high clock-var clock-low node))

(define (make-uuid low mid high clock-var clock-low node)
  ;; Some implementations return upper case of hex
  (string-downcase
   (string-append (pad0l (number->string low 16) 8) "-"
		  (pad0l (number->string mid 16) 4) "-"
		  (pad0l (number->string high 16) 4) "-"
		  (pad0l (number->string clock-var 16) 2)
		  (pad0l (number->string clock-low 16) 2) "-"
		  (pad0l (number->string node 16) 12))))

(define (pad0l s n)
  (define len (string-length s))
  (if (< len n)
      (let-values (((out e) (open-string-output-port)))
	(do ((i 0 (+ i 1)) (c (- n len)))
	    ((= i c))
	  (put-char out #\0))
	(string-append (e) s))
      s))

)
