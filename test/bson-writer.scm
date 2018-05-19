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

(test-end)
