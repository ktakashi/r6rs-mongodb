;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/net/tcp.larceny.sls - TCP Socket (Larceny)
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

;; the vanilla version uses SRFI-106
#!r6rs
(library (mongodb net tcp)
    (export tcp-connect)
    (import (rnrs)
	    (prefix (mongodb net socket) socket:)
	    (primitives r5rs:require get-service-by-name make-client-socket
			socket-input-port socket-output-port))

(define (tcp-connect host service)
  (let ((port (or (string->number service 10)
		  (let-values (((port . _) (get-service-by-name service)))
		    port))))
    (let ((s (make-client-socket host port)))
      (socket:make-socket s
			  (lambda ()
			    (close-input-port (socket-input-port s))
			    (close-output-port (socket-output-port s)))
			  (socket-input-port s)
			  (socket-output-port s)))))

(r5rs:require 'socket)
)

    
