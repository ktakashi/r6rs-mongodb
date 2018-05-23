;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/bson/parser.sls - BSON parser
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
(library (mongodb bson parser)
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
	    (mongodb util bytevectors)
	    (mongodb util ports)
	    (mongodb bson conditions)
	    (mongodb bson validators))

;;; BSON format in S-expr
;;; document ::= e-list
;;; e-list   ::= (element e-list)
;;; element  ::= (e-name number)
;;;            | (e-name string)
;;;            | (e-name e-list)
;;;            | (e-name)
;;;            ... so on
(define (read-document in)
  (define (read-e-list in size)
    (let loop ((r '()) (read 0))
      (if (= read size)
	  (reverse r)
	  (let-values (((size element) (read-element in)))
	    (loop (cons element r) (+ read size))))))
  (let* ((document-size (read-int32 in))
	 (r (read-e-list in (- document-size 5))))
    (unless (eqv? (get-u8 in) 0)
      (raise-bson-read-error 'bson-read "BSON document must end with 0"))
    (values document-size r)))

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
    (values size `(,name min-key))))
(define (read-max-key-element in)
  (let-values (((size name) (read-cstring in)))
    (values size `(,name max-key))))

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
  (let-values (((size name) (read-cstring in)))
    (let ((bv (read-n-bytevector in 12)))
      (values (+ size 12) `(,name (object-id ,(bytevector->hex-string bv)))))))

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
  (let*-values (((esize name)  (read-cstring in))
		((rsize regex) (read-cstring in))
		((fsize flags) (read-cstring in)))
    (values (+ esize rsize fsize)
	    (list name `(regex ,regex
			       ,(check-regex-flag-order 'bson-read flags))))))

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
    (values (+ esize ssize) `(,name (symbol ,symbol)))))

(define (read-javascript/scope-element in)
  (let*-values (((esize name)   (read-cstring in))
		((ssize script) (read-string in))
		((size scope)   (read-document in)))
    (values (+ esize ssize size)
	    (list name `(javascript/scope ,script ,scope)))))

(define (read-int32-element in)
  (let-values (((esize name)   (read-cstring in)))
    (values (+ esize 4) (list name `(s32 ,(read-int32 in))))))
(define (read-uint64-element in)
  (let-values (((esize name)   (read-cstring in)))
    (values (+ esize 8) (list name `(u64 ,(read-uint64 in))))))
(define (read-int64-element in)
  (let-values (((esize name)   (read-cstring in)))
    (values (+ esize 8) (list name `(s64 ,(read-int64 in))))))
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


(define (read-binary in)
  (let* ((size (read-int32 in))
	 (subtype (get-u8 in)))
    (when (eof-object? subtype)
       (raise-bson-read-error 'bson-read "Unexpected EOF"))
    ;; For now just return subtype as it is
    ;; the spec says almost nothing, (e.g. wtf is Function?)
    (values (+ size 5) `(binary ,subtype ,(read-n-bytevector in size)))))

;; helpers
(define-syntax define-helper
  (syntax-rules ()
    ((_ (name args ...) aggregate)
     (define (name args ...)
       (guard (e (else (raise (condition
			       (make-bson-error)
			       e))))
	 (aggregate args ...))))))

(define-helper (read-cstring in) get-cstring)
(define-helper (read-string in) get-string)
(define-helper (read-int32 in) 	get-s32)
(define-helper (read-f64 in)   	get-f64)
(define-helper (read-int64 in) 	get-s64)
(define-helper (read-uint64 in) get-u64)
(define-helper (read-n-bytevector in n) get-n-bytevector)

(define (raise-bson-read-error who msg . irr)
  (raise (condition
	  (make-bson-error)
	  (make-i/o-read-error)
	  (make-who-condition who)
	  (make-message-condition msg)
	  (make-irritants-condition irr))))
)
