;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/util/iso-date.sls - ISODate utilitiy
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
(library (mongodb util iso-date)
    (export milliseconds->iso-date-string
	    iso-date-string->milliseconds)
    (import (rnrs))

;; in MongoDB all integers are little endian
(define (milliseconds->iso-date-string mills)
  (define second (div mills 1000))
  (define frag   (mod mills 1000))
  (let-values (((secs date month year)
		(decode-julian-day-number (seconds->julian-day-number second))))
    (let* ((hours    (div secs (* 60 60)))
	   (rem      (mod secs (* 60 60)))
	   (minutes  (div rem 60))
	   (seconds  (mod rem 60)))
      (let ((s (string-append (number->string year) "-"
			      (pad0l (number->string month) 2) "-"
			      (pad0l (number->string date) 2) "T"
			      (pad0l (number->string hours) 2) ":"
			      (pad0l (number->string minutes) 2) ":"
			      (pad0l (number->string seconds) 2))))
	(if (zero? frag)
	    (string-append s "Z")
	    (string-append s  "." (pad0l (number->string frag) 3) "Z"))))))

(define (iso-date-string->milliseconds s)
  (define len (string-length s))
  (define (err)
    (assertion-violation 'iso-date-string->milliseconds
			 "Invalid format of ISO date" s))
  (define (check-char s index expected)
    (unless (eqv? expected (string-ref s index)) (err)))
  (define (string->number/check s)
    (string-for-each
     (lambda (c) (unless (and (char<? c #\x7F) (char-numeric? c)) (err))) s)
    (string->number s))
  (define (parse-time s year month date)
    (define (parse-milliseconds s hour min sec)
      (define last (if (eqv? (string-ref s (- len 1)) #\Z) (- len 1) len))
      (let ((millis (substring s 20 last)))
	(->utc-datetime year month date hour min sec
			(case (string-length millis)
			  ;; handling, I think, the most used cases
			  ((0) (err))
			  ((1) (* (string->number/check millis) 100))
			  ((2) (* (string->number/check millis) 10))
			  ((3) (string->number/check millis))
			  ;; the rest can be like this...
			  (else (string->number/check
				 (substring millis 0 3)))))))
    (when (< len 19) (err))
    (check-char s 10 #\T)
    (let ((hour (string->number/check (substring s 11 13)))
	  (min (string->number/check (substring s 14 16)))
	  (sec (string->number/check (substring s 17 19))))
      (check-char s 13 #\:)
      (check-char s 16 #\:)
      (if (= len 19)
	  (->utc-datetime year month date hour min sec 0)
	  (case (string-ref s 19)
	    ((#\.) (parse-milliseconds s hour min sec))
	    ((#\Z) (->utc-datetime year month date hour min sec 0))
	    (else (err))))))
  (when (< len 10) (err))
  (let ((year (string->number/check (substring s 0 4)))
	(month (string->number/check (substring s 5 7)))
	(date (string->number/check (substring s 8 10))))
    (check-char s 4 #\-)
    (check-char s 7 #\-)
    (if (> len 10)
	(parse-time s year month date)
	(->utc-datetime year month date 0 0 0 0))))

(define tai-epoch-in-jd 4881175/2)
(define (->utc-datetime year month day hour minute second millis)
  (let ((jdays (- (encode-julian-day-number day month year) tai-epoch-in-jd)))
    (+ (* (+ (* (- jdays 1/2) 24 60 60) (* hour 60 60) (* minute 60) second)
	  1000)
       millis)))

(define (decode-julian-day-number jdn)
  (let* ((days (truncate jdn))
	 (a (+ days 32044))
	 (b (div (+ (* 4 a) 3) 146097))
	 (c (- a (div (* 146097 b) 4)))
	 (d (div (+ (* 4 c) 3) 1461))
	 (e (- c (div (* 1461 d) 4)))
	 (m (div (+ (* 5 e) 2) 153))
	 (y (+ (* 100 b) d -4800 (div m 10))))
    (values ; seconds date month year
     (* (- jdn days) 86400)
     (+ e (- (div (+ (* 153 m) 2) 5)) 1)
     (+ m 3 (* -12 (div m 10)))
     (if (>= 0 y) (- y 1) y))))

(define (encode-julian-day-number day month year)
  (let* ((a (div (- 14 month) 12))
	 (y (- (- (+ year 4800) a) (if (negative? year) -1 0)))
	 (m (- (+ month (* 12 a)) 3)))
    (+ day
       (div (+ (* 153 m) 2) 5)
       (* 365 y)
       (div y 4)
       (- (div y 100))
       (div y 400)
       -32045)))

(define (seconds->julian-day-number seconds)
  (+ (/ (+ seconds 43200) 86400) tai-epoch-in-jd))

;; ... copy&paste...
;; FIXME
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
