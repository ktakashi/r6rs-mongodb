;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/bson.sls - BSON
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

;; user level library

#!r6rs
(library (mongodb bson)
    (export bson-write
	    bson-read
	    bson-error?)
    (import (rnrs)
	    (mongodb bson parser)
	    (mongodb bson writer)
	    (mongodb bson conditions))

;;; API
(define bson-write
  (case-lambda
   ((bson) (bson-write bson (current-output-port)))
   ((bson out)
    (unless (or (null? bson) (pair? bson))
      (assertion-violation 'bson-write "BSON document must a list" bson))
    (unless (and (binary-port? out) (output-port? out))
      (assertion-violation 'bson-write "Binary output port required" out))
    (write-document out bson))))

;;; API
(define bson-read
  (case-lambda
   (() (bson-read (current-input-port)))
   ((in)
    (unless (and (binary-port? in) (input-port? in))
      (assertion-violation 'bson-read "Binary input port required" in))
    (let-values (((size bson) (read-document in)))
      bson))))
)
