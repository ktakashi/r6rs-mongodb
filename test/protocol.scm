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
    (test-equal "abc" (mongodb-query-message-full-collection-name msg))
    (test-equal 0 (mongodb-flagged-query-message-flags msg))

    (let-values (((out extract) (open-bytevector-output-port)))
      (test-assert "write-mongodb-message" (write-mongodb-message out msg))
      (test-equal "compare op-update" op-update-message (extract)))))

(let ()
  (define op-insert-message
    (->message #vu8(0 0 0 0
		    #x61 #x62 #x63 #x00
		    #x05 #x00 #x00 #x00 #x00
		    #x05 #x00 #x00 #x00 #x00)
	       *op-code:insert*))
  (test-assert (op-insert? (read-mongodb-message (bin op-insert-message))))
  (let ((msg (read-mongodb-message (bin op-insert-message))))
    (test-assert (mongodb-protocol-message? msg))
    (test-assert (msg-header? (mongodb-protocol-message-header msg)))
    (test-equal "abc" (mongodb-query-message-full-collection-name msg))
    (test-equal 0 (mongodb-flagged-query-message-flags msg))
    (test-equal '#(() ()) (op-insert-documents msg))
    
    (let-values (((out extract) (open-bytevector-output-port)))
      (test-assert "write-mongodb-message" (write-mongodb-message out msg))
      (test-equal "compare op-insert" op-insert-message (extract)))))

(let ()
  (define op-query-message
    (->message #vu8(0 0 0 0
		    #x61 #x62 #x63 #x00
		    0 0 0 0
		    0 0 0 0
		    #x05 #x00 #x00 #x00 #x00)
	       *op-code:query*))
  (define op-query-message/selector
    (->message #vu8(0 0 0 0
		    #x61 #x62 #x63 #x00
		    0 0 0 0
		    0 0 0 0
		    #x05 #x00 #x00 #x00 #x00
		    #x05 #x00 #x00 #x00 #x00)
	       *op-code:query*))
  (define (test-op-query op-query-message has-return?)
    (test-assert (op-query? (read-mongodb-message (bin op-query-message))))
    (let ((msg (read-mongodb-message (bin op-query-message))))
      (test-assert (mongodb-protocol-message? msg))
      (test-assert (msg-header? (mongodb-protocol-message-header msg)))
      (test-equal "abc" (mongodb-query-message-full-collection-name msg))
      (test-equal "flag" 0 (mongodb-flagged-query-message-flags msg))
      (test-equal "number to skip" 0 (op-query-number-to-skip msg))
      (test-equal "number to return "0 (op-query-number-to-return msg))
      (test-equal "query" '() (op-query-query msg))
      (if has-return?
	  (test-equal "return fields selector"
		      '() (op-query-return-fields-selector msg))
	  (test-equal "return fields selector"
		      #f (op-query-return-fields-selector msg)))
      (let-values (((out extract) (open-bytevector-output-port)))
	(test-assert "write-mongodb-message" (write-mongodb-message out msg))
	(test-equal "compare op-query" op-query-message (extract)))))
  (test-op-query op-query-message #f)
  (test-op-query op-query-message/selector #t))
  

(test-end)
