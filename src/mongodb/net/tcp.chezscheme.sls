;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/net/tcp.chezscheme.sls - TCP Socket (Chez)
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
(library (mongodb net tcp)
    (export tcp-connect)
    (import (rnrs)
	    (only (chezscheme)
		  foreign-procedure machine-type load-shared-object
		  open-fd-input-port open-fd-output-port
		  library-directories)
	    (prefix (mongodb net socket) socket:))
(define (find-file file)
  (let loop ((dirs (library-directories)))
    (if (null? dirs)
	(assertion-violation 'tcp-connect "Couldn't find runtime" file)
	(let ((path (string-append (caar dirs) "/" file)))
	  (if (file-exists? path)
	      path
	      (loop (cdr dirs)))))))
(define dummy 
  (begin
    (load-shared-object (find-file "mongodb/net/tcp/chez.so"))
    (case (machine-type)
      ((a6le i3le ti3le) (load-shared-object "libc.so.6"))
      ((i3osx ti3osx a6osx ta6osx) (load-shared-object "libc.dylib"))
      (else (load-shared-object "libc.so")))))

    
(define close (foreign-procedure "close" (int) int))
(define make-client-socket
  (foreign-procedure "make_client_socket" (string string) int))
(define socket-shutdown (foreign-procedure "socket_shutdown" (int) int))

(define (tcp-connect host service)
  (let ((s (make-client-socket host service)))
    (socket:make-socket (lambda ()
			  (socket-shutdown s)
			  (close s))
			(open-fd-input-port s 'none)
			(open-fd-output-port s))))
)
