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

(test-write "write-string (1)" #vu8(#x04 #x00 #x00 #x00 #x61 #x62 #x63 #x00)
	    write-string "abc")
(test-write "write-string (2)" #vu8(#x05 #x00 #x00 #x00 #x61 #x62 #x00 #x63 #x00)
	    write-string "ab\x0;c")

(test-write "write-double (1)" #vu8(#x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)
	    write-double 3.14)

(test-write "write-min-key-element (1)" #vu8(#xFF #x61 #x62 #x63 #x00)
	    write-min-key-element '("abc" min-key))
(test-write "write-min-key-element (2)" #vu8(#xFF #x61 #x62 #x63 #x00)
	    write-element '("abc" min-key))

(test-write "write-max-key-element (1)" #vu8(#x7F #x61 #x62 #x63 #x00)
	    write-max-key-element '("abc" max-key))
(test-write "write-max-key-element (2)" #vu8(#x7F #x61 #x62 #x63 #x00)
	    write-element '("abc" max-key))

(test-write "write-double-element (1)"
	    #vu8(#x01
		 #x61 #x62 #x63 #x00 #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)
	    write-double-element '("abc" 3.14))
(test-write "write-double-element (2)"
	    #vu8(#x01
		 #x61 #x62 #x63 #x00 #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)
	    write-element '("abc" 3.14))

(test-write "write-string-element (1)"
	    #vu8(#x02
		 #x61 #x62 #x63 #x00 #x04 #x00 #x00 #x00 #x61 #x62 #x63 #x00)
	    write-string-element '("abc" "abc"))
(test-write "write-string-element (2)"
	    #vu8(#x02
		 #x61 #x62 #x63 #x00 #x04 #x00 #x00 #x00 #x61 #x62 #x63 #x00)
	    write-element '("abc" "abc"))

(test-write "write-embedded-document-element (1)"
	    #vu8(#x03
		 #x61 #x62 #x63 #x00
		 #x0F #x00 #x00 #x00
		 #xFF #x61 #x62 #x63 #x00
		 #x7F #x61 #x62 #x63 #x00 #x00)
	    write-embedded-document-element
	    '("abc" (("abc" min-key) ("abc" max-key))))
(test-write "write-embedded-document-element (2)"
	    #vu8(#x03
		 #x61 #x62 #x63 #x00
		 #x0F #x00 #x00 #x00
		 #xFF #x61 #x62 #x63 #x00
		 #x7F #x61 #x62 #x63 #x00 #x00)
	    write-element
	    '("abc" (("abc" min-key) ("abc" max-key))))

(test-write "write-array-element (1)"
	    #vu8(#x04
		 #x61 #x62 #x63 #x00
		 #x1B #x00 #x00 #x00
		 #x01 #x30 #x00 ;; "0"
		 #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40
		 #x02 #x31 #x00 ;; "1"
		 #x04 #x00 #x00 #x00 #x61 #x62 #x63 #x00
		 #x00)
	    write-array-element '("abc" #(3.14 "abc")))
(test-write "write-array-element (2)"
	    #vu8(#x04
		 #x61 #x62 #x63 #x00
		 #x1B #x00 #x00 #x00
		 #x01 #x30 #x00 ;; "0"
		 #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40
		 #x02 #x31 #x00 ;; "1"
		 #x04 #x00 #x00 #x00 #x61 #x62 #x63 #x00
		 #x00)
	    write-element '("abc" #(3.14 "abc")))

(test-end)
