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
	    read-undefined-element
	    read-object-id-element
	    read-boolean-element
	    read-utc-datetime-element
	    read-null-element
	    read-regex-element
	    read-db-pointer-element
	    read-javascript-element
	    read-symbol-element
	    read-javascript/scope-element
	    read-int32-element
	    read-uint64-element
	    read-int64-element
	    read-decimal128-element

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

; Deprecated - but need to be supported I guess
(define (read-undefined-element in)
  (let-values (((size name) (read-cstring in)))
    (values size (list name 'undefined))))

(define (read-object-id-element in)
  (define (read-counter bv)
    (bitwise-ior (bitwise-arithmetic-shift-left (bytevector-u8-ref bv 9) 16)
		 (bitwise-arithmetic-shift-left (bytevector-u8-ref bv 10) 8)
		 (bytevector-u8-ref bv 11)))
  (define (bytevector-copy-n src start count)
    (define bv (make-bytevector count))
    (bytevector-copy! src start bv 0 count)
    bv)
  (define (parse-object-id bv)
    `(object-id ,(bytevector-u32-ref bv 0 (endianness big)) ;; Unix epoch sec
		,(bytevector-copy-n bv 4 3) ;; machine identifier
		,(bytevector-u16-ref bv 7 (endianness big)) ;; process id
		,(read-counter bv)))
  (let-values (((size name) (read-cstring in)))
    (let ((bv (read-n-bytevector in 12)))
      (values (+ size 12) (list name (parse-object-id bv))))))

(define (read-boolean-element in)
  (let-values (((size name) (read-cstring in)))
    (let ((b (get-u8 in)))
      (values (+ size 1)
	      (list name (case b
			   ((#x00) #f)
			   ((#x01) #t)
			   (else (raise-bson-read-error
				  'bson-read "Unknown boolean value" b))))))))

(define (read-utc-datetime-element in)
  (let-values (((size name) (read-cstring in)))
    (values (+ size 8) (list name `(utc-datetime ,(read-int64 in))))))

(define (read-null-element in)
  (let-values (((size name) (read-cstring in)))
    (values size (list name 'null))))

(define (read-regex-element in)
  (define valid-flags '(#\i #\l #\m #\s #\u #\x))
  (define (check-order flags)
    (let loop ((check (string->list flags)) (compare valid-flags))
      (cond ((null? check) flags)
	    ((memq (car check) compare) =>
	     (lambda (rest) (loop (cdr check) (cdr rest))))
	    (else (raise-bson-read-error
		   'bson-read "Unknown flags or invalid order" flags)))))
  (let*-values (((esize name)  (read-cstring in))
		((rsize regex) (read-cstring in))
		((fsize flags) (read-cstring in)))
    (values (+ esize rsize fsize)
	    (list name `(regex ,regex ,(check-order flags))))))

;; Deprecated but need to support...
(define (read-db-pointer-element in)
  (let*-values (((esize name)    (read-cstring in))
		((psize pointer) (read-string in)))
    (values (+ esize psize 12)
	    (list name `(db-pointer ,pointer ,(read-n-bytevector in 12))))))

(define (read-javascript-element in)
  (let*-values (((esize name)   (read-cstring in))
		((ssize script) (read-string in)))
    (values (+ esize ssize) (list name `(javascript ,script)))))

;; Deprecated but need to support...
(define (read-symbol-element in)
  (let*-values (((esize name)   (read-cstring in))
		((ssize symbol) (read-string in)))
    (values (+ esize ssize) (list name (string->symbol symbol)))))

(define (read-javascript/scope-element in)
  (let*-values (((esize name)   (read-cstring in))
		((ssize script) (read-string in))
		((size scope)   (read-document in)))
    (values (+ esize ssize size)
	    (list name `(javascript/scope ,script ,scope)))))

(define (read-int32-element in)
  (let-values (((esize name)   (read-cstring in)))
    (values (+ esize 4) (list name `(int32 ,(read-int32 in))))))
(define (read-uint64-element in)
  (let-values (((esize name)   (read-cstring in)))
    (values (+ esize 8) (list name `(uint64 ,(read-uint64 in))))))
(define (read-int64-element in)
  (let-values (((esize name)   (read-cstring in)))
    (values (+ esize 8) (list name `(int64 ,(read-int64 in))))))
(define (read-decimal128-element in)
  (raise-bson-read-error 'bson-read "Decimal 128 is not supported"))

(define *dispatch-table*
  `#(,read-double-element
     ,read-string-element
     ,read-embedded-document-element
     ,read-array-element
     ,read-binary-element
     ,read-undefined-element
     ,read-object-id-element
     ,read-boolean-element
     ,read-utc-datetime-element
     ,read-null-element
     ,read-regex-element
     ,read-db-pointer-element
     ,read-javascript-element
     ,read-symbol-element
     ,read-javascript/scope-element
     ,read-int32-element
     ,read-uint64-element
     ,read-int64-element
     ,read-decimal128-element
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
	 (s    (read-n-bytevector in (- size 1)))
	 (end-mark (get-u8 in)))
    (unless (eqv? end-mark 0)
      (raise-bson-read-error 'bson-read "String must be followed by 0"
			     end-mark))
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
(define (read-int64 in)
  (let ((bv (read-n-bytevector in 8)))
    (bytevector-s64-ref bv 0 (endianness little))))
(define (read-uint64 in)
  (let ((bv (read-n-bytevector in 8)))
    (bytevector-u64-ref bv 0 (endianness little))))

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
