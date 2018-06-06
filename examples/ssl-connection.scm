#|
Updating socket to TLS socket

This example works only on Sagittarius Scheme
|#
(import (rnrs)
	(rfc tls)
	(mongodb)
	(mongodb net socket)
	(prefix (sagittarius socket) socket:))

(define (socket-converter socket)
  (let ((tls-socket (socket->tls-socket (socket-raw-socket socket)
					:handshake #t
					:client-socket #t)))
    (make-socket tls-socket
		 (lambda ()
		   (tls-socket-shutdown tls-socket socket:SHUT_RDWR)
		   (tls-socket-close tls-socket))
		 (tls-socket-input-port tls-socket)
		 (tls-socket-output-port tls-socket))))

(define connection-option
  (make-mongodb-connection-option
   (make-mongodb-connection-option-default-request-id-strategy)
   socket-converter
   #t))

(call-with-mongodb-database "localhost" 27017 "test"
  (lambda (database)
    (mongodb-query-for-each
     (lambda (doc) (display doc) (newline))
     (mongodb-database-query database "testCollection" '())))
  connection-option)
    
