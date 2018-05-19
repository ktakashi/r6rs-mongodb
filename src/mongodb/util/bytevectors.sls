;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/util/bytevectors.sls - Bytevector utilities
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

(library (mongodb util bytevectors)
    (export bytevector->hex-string)
    (import (rnrs)
	    (rnrs mutable-strings))

(define (bytevector->hex-string bv . maybe-upper?)
  (define upper? (and (pair? maybe-upper?) (car maybe-upper?)))
  (define (hex->char i)
    (cond ((< i 10) (integer->char (+ i 48)))   ;; + #\0
	  (upper?   (integer->char (+ i 55)))   ;; + #\A - 10
	  (else     (integer->char (+ i 87))))) ;; + #\a - 10
  (let* ((len (bytevector-length bv))
	 (str (make-string (* len 2))))
    (do ((i 0 (+ i 1)))
	((= i len) str)
      (let* ((b (bytevector-u8-ref bv i))
	     (hi (bitwise-arithmetic-shift (bitwise-and b #xF0) -4))
	     (lo (bitwise-and b #x0F)))
	;; we use string-set! here since on R6RS it's O(1)
	(string-set! str (* i 2) (hex->char hi))
	(string-set! str (+ (* i 2) 1) (hex->char lo))))))
)

