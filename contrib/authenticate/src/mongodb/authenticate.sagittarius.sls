;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; mongodb/authenticate.sagittarius.sls - MongoDB authentication
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

#!core
#!read-macro=sagittarius/bv-string
(library (mongodb authenticate)
    (export mongodb-database-authenticate!
	    mongodb-database-logout!
	    mongodb-authenticate-error?)
    (import (rnrs)
	    (sagittarius)
	    (mongodb database)
	    (mongodb authenticate conditions)
	    (mongodb authenticate logout)
	    (sagittarius regex)
	    (rfc hmac)
	    (rfc base64)
	    (rsa pkcs :5)
	    (math)
	    (util bytevector)
	    (srfi :13))

;; As it's recommended, we implement the SCRAM method only.
;; reference: https://tools.ietf.org/html/rfc5802
;; TODO maybe we should port SCRAM to Sagittarius itself as well...
(define (mongodb-database-authenticate! database username password)
  (define nonce (bytevector->hex-string
		 ;; how many bytes should we use?
		 (read-random-bytes (secure-random ChaCha20) 16)))
  (define (unwrap-binary binary) (caddr (cadr binary)))
  (define (parse-response document)
    (unless (assoc "ok" document)
      (mongodb-authenticate-error 'mongodb-database-authenticate!
				  "Invalid response"
				  document))
    (cond ((assoc "errmsg" document) =>
	   (lambda (slot)
	     (mongodb-authenticate-error 'mongodb-database-authenticate!
					 (cadr slot)
					 document))))
    (values (cond ((assoc "done" document) => cadr))
	    (cond ((assoc "conversationId" document) => cadr))
	    (cond ((assoc "payload" document) => unwrap-binary))))
  (define (parse-server-response response)
    (let ((tokens (string-split response #\,)))
      (map (lambda (token) (string-split token #\=)) tokens)))
  (define (start msg)
    (mongodb-database-run-command database
     `(("saslStart" (s32 1))
       ("mechanism" "SCRAM-SHA-1")
       ("payload" (binary 0 ,(string->utf8 msg))))))
  (define (continue id msg)
    (mongodb-database-run-command database
     `(("saslContinue" (s32 1))
       ("conversationId" ,id)
       ("payload" (binary 0 ,(string->utf8 msg))))))
  (define (get/error key message)
    (cond ((assoc key message) => cadr)
	  (else (mongodb-authenticate-error 'mongodb-database-authenticate!
					    "Invalid server response"))))
  
  (define (c0 username) (string-append "n=" username ",r=" nonce))
  
  (define (c1 server-challenge username password initial)
    (define (check-nonce r)
      (unless (string-prefix? nonce r)
	(mongodb-authenticate-error 'mongodb-database-authenticate!
				    "Invalid server nonce"))
      r)
    (define message (parse-server-response server-challenge))
    (define r (check-nonce (get/error "r" message)))
    (define s (get/error "s" message))
    (define i (get/error "i" message))
    (let* ((channel-binding (string-append "c=" (base64-encode-string "n,,")))
	   (nonce (string-append "r=" r))
	   (wo/proof (string-append channel-binding "," nonce)))
      (define salted-password
	(hi (authentication-hash username password)
	    (base64-decode-string s :transcoder #f)
	    (string->number i)))
      (define client-key (hmac salted-password #*"Client Key"))
      (define stored-key (hash SHA-1 client-key))
      (define auth-message (string->utf8
			     (string-append initial "," server-challenge ","
					    wo/proof)))
      (define client-signature (hmac stored-key auth-message))
      (define client-proof (bytevector-xor client-key client-signature))
      (define server-key (hmac salted-password #*"Server Key"))
      (define server-signature (hmac server-key auth-message))
      (values (string-append wo/proof
			     ",p=" (utf8->string (base64-encode client-proof)))
	      server-signature)))
  
  (define (c2 challenge computed-signature)
    (define message (parse-server-response challenge))
    (define v (get/error "v" message))
    (unless (bytevector=? (base64-decode-string v :transcoder #f)
			  computed-signature)
      (mongodb-authenticate-error 'mongodb-database-authenticate!
				  "Invalid signature"))
    v)
  
  (let* ((initial (c0 username))
	 (response (start (string-append "n,," initial))))
    (let*-values (((done? conversation-id payload) (parse-response response))
		  ((msg signature) (c1 (utf8->string payload)
				       username password initial)))
      (let ((response (continue conversation-id msg)))
	(let-values (((done? conversation-id payload)
		      (parse-response response)))
	  (continue conversation-id (c2 (utf8->string payload) signature)))))))

(define (authentication-hash username password)
  (bytevector->hex-string
   (hash MD5 (string->utf8 (string-append username ":mongo:" password)))
   :upper? #f))

(define (hi key salt iterations)
  ;; dkLen == length of HMAC output == SHA-1 output == 20
  (pbkdf-2 (string->utf8 key) salt iterations 20))
;; why...
(define (hmac bytes key) (hash HMAC key :key bytes))

(define (mongodb-authenticate-error who msg . irr)
  (raise (condition (make-mongodb-authenticate-error)
		    (make-who-condition who)
		    (make-message-condition msg)
		    (make-irritants-condition irr))))
)
