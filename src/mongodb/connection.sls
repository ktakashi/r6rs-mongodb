;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/connection.sls - MongoDB connection
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
(library (mongodb connection)
    (export mongodb-connection? make-mongodb-connection
	    open-mongodb-connection!
	    close-mongodb-connection!
	    mongodb-connection-open?
	    mongodb-connection-input-port
	    mongodb-connection-output-port

	    mongodb-connection-run-command
	    mongodb-connection-list-databases
	    

	    &mongodb-connection
	    mongodb-connection-error? make-mongodb-connection-error
	    &mongodb-connection-closed make-mongodb-connection-closed
	    mongodb-connection-closed?
	    )
    (import (rnrs)
	    (mongodb conditions)
	    (mongodb net socket)
	    (mongodb net tcp)
	    (mongodb protocol))

(define-record-type mongodb-connection
  (fields host
	  port
	  (mutable socket)
	  ;; per connection should be fine right?
	  (mutable request-id))
  (protocol (lambda (p)
	      (lambda (host . maybe-port)
		;; default port is 27017
		(define port (if (null? maybe-port) 27017 (car maybe-port)))
		(unless (string? host)
		  (assertion-violation 'make-mongodb-connection
				       "Host must be a string" host))
		(unless (and (fixnum? port) (positive? port))
		  (assertion-violation 'make-mongodb-connection
				       "Port must be positive fixnum" port))
		(p host (number->string port) #f 1)))))

(define-condition-type &mongodb-connection &mongodb
  make-mongodb-connection-error mongodb-connection-error?)
(define-condition-type &mongodb-connection-closed &mongodb-connection
  %make-mongodb-connection-closed mongodb-connection-closed?)
(define (make-mongodb-connection-closed)
  (condition (%make-mongodb-connection-closed)
	     (make-i/o-error)))

(define (mongodb-connection-request-id! conn)
  (let ((id (mongodb-connection-request-id conn)))
    (mongodb-connection-request-id-set! conn (+ id 1))
    id))

(define (open-mongodb-connection! connection)
  (let ((socket (tcp-connect (mongodb-connection-host connection)
			     (mongodb-connection-port connection))))
    (mongodb-connection-socket-set! connection socket)
    connection))

(define (close-mongodb-connection! connection)
  (let ((socket (mongodb-connection-socket connection)))
    (when socket
      (socket-close! socket)
      (mongodb-connection-socket-set! connection #f))
    connection))

(define (mongodb-connection-open? connection)
  (and (mongodb-connection-socket connection) #t))

(define (mongodb-connection-input-port connection)
  (check-socket-open 'mongodb-connection-input-port connection)
  (socket-input-port (mongodb-connection-socket connection)))

(define (mongodb-connection-output-port connection)
  (check-socket-open 'mongodb-connection-output-port connection)
  (socket-output-port (mongodb-connection-socket connection)))

(define (mongodb-connection-run-command conn command)
  (define (->bson command)
    (if (string? command)
	`((,command 1))
	command))
  (check-socket-open 'mongodb-connection-run-command conn)
  (let ((q (make-op-query "admin.$cmd" 1 1 (->bson command) #f))
	(sock (mongodb-connection-socket conn)))
    (mongodb-protocol-message-request-id-set! q
     (mongodb-connection-request-id! conn))
    (write-mongodb-message (socket-output-port sock) q)
    (let ((op-reply (read-mongodb-message (socket-input-port sock))))
      (vector-ref (op-reply-documents op-reply) 0))))

(define (mongodb-connection-list-databases conn)
  (mongodb-connection-run-command conn "listDatabases"))

(define (check-socket-open who con)
  (unless (mongodb-connection-open? con)
    (raise (condition (make-mongodb-connection-closed)
		      (make-who-condition who)
		      (make-message-condition "Connection is not open")
		      (make-irritants-condition con)))))

)
