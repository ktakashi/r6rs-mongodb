#!r6rs
(import (rnrs)
	(mongodb bson writer)
	(mongodb bson conditions)
	(srfi :64))

(test-begin "BSON writer")

(define (test-write name expect proc value)
  (let-values (((out extract) (open-bytevector-output-port)))
    (test-equal name expect (begin (proc out value) (extract)))))

(test-write "write-cstring (1)" #vu8(#x61 #x62 #x63 #x00) write-cstring "abc")
(test-error "write-cstring (2)" bson-error? (write-cstring #f "ab\x0;c"))

(test-write "write-double (1)" #vu8(#x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)
	    write-double 3.14)

(test-write "write-min-key-element" #vu8(#xFF #x61 #x62 #x63 #x00)
	    write-min-key-element '("abc" min-key))
(test-write "write-max-key-element" #vu8(#x7F #x61 #x62 #x63 #x00)
	    write-max-key-element '("abc" max-key))

(test-write "write-double-element"
	    #vu8(#x01 #x61 #x62 #x63 #x00 #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)
	    write-double-element '("abc" 3.14))

(test-end)
