;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; binary/bson/parser.sls - BSON parser
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

;; this library is separated for testing purpose
;; reference
;;  - http://bsonspec.org/spec.html
#!r6rs
(library (binary bson parser)
    (export read-document
	    read-element
	    read-min-key-element
	    read-max-key-element
	    read-double-element
	    read-string-element
	    read-embedded-document-element
	    read-array-element
	    read-binary-element

	    read-cstring
	    read-string
	    read-binary)
    (import (rnrs)
	    (binary bson conditions))

;;; BSON format in S-expr
;;; document ::= e-list
;;; e-list   ::= (element e-list)
;;; element  ::= (e-name number)
;;;            | (e-name string)
;;;            | (e-name e-list)
;;;            | (e-name)
;;;            ... so on
(define (read-document in)
  (let* ((document-size (read-int32 in))
	 (r (read-e-list in (- document-size 5))))
    (unless (eqv? (get-u8 in) 0)
      (raise-bson-read-error 'bson-read "BSON document must end with 0"))
    (values document-size r)))

;; it's sort of helper so not exported
(define (read-e-list in size)
  (let loop ((r '()) (read 0))
    (if (= read size)
	(reverse r)
	(let-values (((size element) (read-element in)))
	  (loop (cons element r) (+ read size))))))

(define (read-element in)
  (define (read-by-type in type)
    (cond ((eof-object? type)
	   (raise-bson-read-error 'bson-read "Unexpected EOF"))
	  ((= type #xFF) (read-min-key-element in))
	  ((= type #x7F) (read-max-key-element in))
	  (else ((vector-ref *dispatch-table* (- type 1)) in))))
  (let ((type (get-u8 in)))
    (let-values (((size e) (read-by-type in type)))
      (values (+ size 1) e))))

(define (read-min-key-element in)
  (let-values (((size name) (read-cstring in)))
    (values size `(min-key ,name))))
(define (read-max-key-element in)
  (let-values (((size name) (read-cstring in)))
    (values size `(max-key ,name))))

(define (read-double-element in)
  (let-values (((size name) (read-cstring in)))
    (let ((d (read-f64 in)))
      (values (+ size 8) (list name d)))))

(define (read-string-element in)
  (let*-values (((esize name) (read-cstring in))
		((vsize str)  (read-string in)))
    (values (+ esize vsize) (list name str))))

(define (read-embedded-document-element in)
  (let*-values (((esize name) (read-cstring in))
		((vsize doc)  (read-document in)))
    (values (+ esize vsize) (list name doc))))

(define (read-array-element in)
  (define (->vector doc)
    (let loop ((arr doc) (r '()) (current 0))
      (cond ((null? arr) (list->vector (reverse r)))
	    ((eqv? current (string->number (caar arr)))
	     (loop (cdr arr) (cons (cadar arr) r) (+ current 1)))
	    (else
	     (raise-bson-read-error
	      'bson-read
	      "Array must contain numeric key of ascending order"
	      doc)))))
  (let*-values (((esize name) (read-cstring in))
		((vsize doc)  (read-document in)))
    (values (+ esize vsize) (list name (->vector doc)))))

(define (read-binary-element in)
  (let*-values (((esize name) (read-cstring in))
		((vsize bin)  (read-binary in)))
    (values (+ esize vsize) (list name bin))))

(define *dispatch-table*
  `#(,read-double-element
     ,read-string-element
     ,read-embedded-document-element
     ,read-array-element
     ,read-binary-element
     ))

(define (read-cstring in)
  (let-values (((out extract) (open-bytevector-output-port)))
    (let loop ((count 0))
      (let ((u8 (get-u8 in)))
	(cond ((eof-object? u8)
	       (raise-bson-read-error 'bson-read "Unexpected EOF"))
	      ((zero? u8) (values (+ count 1) (utf8->string (extract))))
	      (else (put-u8 out u8) (loop (+ count 1))))))))

(define (read-string in)
  (let* ((size (read-int32 in))
	 (s    (read-n-bytevector in (- size 1))))
    (unless (eqv? (get-u8 in) 0)
      (raise-bson-read-error 'bson-read "String must be followed by 0"))
    (values (+ size 4) (utf8->string s))))

(define (read-binary in)
  (let* ((size (read-int32 in))
	 (subtype (get-u8 in)))
    (when (eof-object? subtype)
       (raise-bson-read-error 'bson-read "Unexpected EOF"))
    ;; For now just return subtype as it is
    ;; the spec says almost nothing, (e.g. wtf is Function?)
    (values (+ size 5) `(binary ,subtype ,(read-n-bytevector in size)))))

;; helpers
(define (read-int32 in)
  (let ((bv (read-n-bytevector in 4)))
    (bytevector-s32-ref bv 0 (endianness little))))
(define (read-f64 in)
  (let ((bv (read-n-bytevector in 8)))
    (bytevector-ieee-double-ref bv 0 (endianness little))))

;; make sure we read required length of bytes in case of socket port
(define (read-n-bytevector in n)
  (define bv (make-bytevector n))
  (let loop ((r n) (i 0))
    (let ((c (get-bytevector-n! in bv i r)))
      (cond ((eof-object? c)
	     (raise-bson-read-error 'bson-read "Unexpected EOF"))
	    ((= c r) bv)
	    (else (loop (- r c) (+ i c)))))))

(define (raise-bson-read-error who msg . irr)
  (raise (condition
	  (make-bson-error)
	  (make-i/o-read-error)
	  (make-who-condition who)
	  (make-message-condition msg)
	  (make-irritants-condition irr))))
)
