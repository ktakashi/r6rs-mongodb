#!r6rs
(import (rnrs)
	(mongodb protocol)
	(mongodb protocol msg-header)
	(mongodb util ports)
	(srfi :64))

(test-begin "Wire Protocol")

(define (->message bv op-code)
  (let-values (((out extract) (open-bytevector-output-port)))
    (put-s32 out (+ (bytevector-length bv) *msg-header-size*))
    (put-s32 out 1)
    (put-s32 out 2)
    (put-s32 out op-code)
    (put-bytevector out bv)
    (extract)))
(define bin open-bytevector-input-port)

(let ()
  (define op-update-message
    (->message #vu8(0 0 0 0
		    #x61 #x62 #x63 #x00
		    0 0 0 0
		    #x05 #x00 #x00 #x00 #x00
		    #x05 #x00 #x00 #x00 #x00)
	       *op-code:update*))
  (test-assert (op-update? (read-mongodb-message (bin op-update-message))))
  (let ((msg (read-mongodb-message (bin op-update-message))))
    (test-assert (mongodb-protocol-message? msg))
    (test-assert (msg-header? (mongodb-protocol-message-header msg)))
    (test-equal "abc" (op-update-full-collection-name msg))))

(test-end)
