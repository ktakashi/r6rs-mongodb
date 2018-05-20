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

(test-write "write-binary-element (1)"
	    #vu8(#x05
		 #x61 #x62 #x63 #x00
		 #x04 #x00 #x00 #x00
		 #x00
		 #x01 #x02 #x03 #x04)
	    write-binary-element '("abc" (binary #x00 #vu8(1 2 3 4))))
(test-write "write-binary-element (2)"
	    #vu8(#x05
		 #x61 #x62 #x63 #x00
		 #x04 #x00 #x00 #x00
		 #x00
		 #x01 #x02 #x03 #x04)
	    write-element '("abc" (binary #x00 #vu8(1 2 3 4))))

(test-write "write-undefined-element (1)"
	    #vu8(#x06 #x61 #x62 #x63 #x00)
	    write-undefined-element '("abc" undefined))
(test-write "write-undefined-element (2)"
	    #vu8(#x06 #x61 #x62 #x63 #x00)
	    write-element '("abc" undefined))

(test-write "write-object-id-element (1)"
	    #vu8(#x07
		 #x61 #x62 #x63 #x00
		 #x5a #xfe #xc5 #xca
		 #x85 #x9a #x71 #xdf
		 #xd7 #x5d #xa8 #xd0)
	    write-object-id-element
	    '("abc" (object-id "5afec5ca859a71dfd75da8d0")))
(test-write "write-object-id-element (2)"
	    #vu8(#x07
		 #x61 #x62 #x63 #x00
		 #x5a #xfe #xc5 #xca
		 #x85 #x9a #x71 #xdf
		 #xd7 #x5d #xa8 #xd0)
	    write-element
	    '("abc" (object-id "5afec5ca859a71dfd75da8d0")))

(test-write "write-boolean-element (1)"
	    #vu8(#x08 #x61 #x62 #x63 #x00 #x00)
	    write-boolean-element '("abc" #f))
(test-write "write-boolean-element (2)"
	    #vu8(#x08 #x61 #x62 #x63 #x00 #x01)
	    write-boolean-element '("abc" #t))
(test-write "write-boolean-element (3)"
	    #vu8(#x08 #x61 #x62 #x63 #x00 #x01)
	    write-element '("abc" #t))

(test-write "write-utc-datetime-element (1)"
	    #vu8(#x09
		 #x61 #x62 #x63 #x00
		 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
	    write-utc-datetime-element '("abc" (utc-datetime 1)))
(test-write "write-utc-datetime-element (2)"
	    #vu8(#x09
		 #x61 #x62 #x63 #x00
		 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
	    write-element '("abc" (utc-datetime 1)))

(test-write "write-null-element (1)"
	    #vu8(#x0A #x61 #x62 #x63 #x00)
	    write-null-element '("abc" null))
(test-write "write-null-element (2)"
	    #vu8(#x0A #x61 #x62 #x63 #x00)
	    write-element '("abc" null))

(test-write "write-regex-element (1)"
	    #vu8(#x0B
		 #x61 #x62 #x63 #x00
		 #x5C #x77 #x00 ;; \w
		 #x69 #x00      ;; i
		 )
	    write-regex-element '("abc" (regex "\\w" "i")))
(test-write "write-regex-element (2)"
	    #vu8(#x0B
		 #x61 #x62 #x63 #x00
		 #x5C #x77 #x00 ;; \w
		 #x00      ;; 
		 )
	    write-regex-element '("abc" (regex "\\w" "")))
(test-write "write-regex-element (3)"
	    #vu8(#x0B
		 #x61 #x62 #x63 #x00
		 #x5C #x77 #x00 ;; \w
		 #x69 #x6C #x00      ;; 
		 )
	    write-regex-element '("abc" (regex "\\w" "il")))
(call-with-bytevector-output-port
 (lambda (out)
   (test-error "write-regex-element (4)" bson-error?
	       (write-regex-element out '("abc" (regex "\\w" "li"))))
   (test-error "write-regex-element (5)" bson-error?
	       (write-regex-element out '("abc" (regex "\\w" "f"))))))

(test-write "write-db-pointer-element (1)"
	    #vu8(#x0C
		 #x61 #x62 #x63 #x00
		 #x02 #x00 #x00 #x00 #x70 #x00 ;; p
		 1 2 3 4 5 6 7 8 9 10 11 12)
	    write-db-pointer-element
	    '("abc" (db-pointer "p" #vu8(1 2 3 4 5 6 7 8 9 10 11 12))))
(test-write "write-db-pointer-element (2)"
	    #vu8(#x0C
		 #x61 #x62 #x63 #x00
		 #x02 #x00 #x00 #x00 #x70 #x00 ;; p
		 1 2 3 4 5 6 7 8 9 10 11 12)
	    write-element
	    '("abc" (db-pointer "p" #vu8(1 2 3 4 5 6 7 8 9 10 11 12))))

(test-write "write-javascript-element (1)"
	    #vu8(#x0D
		 #x61 #x62 #x63 #x00
		 #x0A #x00 #x00 #x00 
		 #x76 #x61 #x72 #x20 #x6e #x20 #x3d #x20 #x31
		 #x00)
	    write-javascript-element '("abc" (javascript "var n = 1")))
(test-write "write-javascript-element (2)"
	    #vu8(#x0D
		 #x61 #x62 #x63 #x00
		 #x0A #x00 #x00 #x00 
		 #x76 #x61 #x72 #x20 #x6e #x20 #x3d #x20 #x31
		 #x00)
	    write-element '("abc" (javascript "var n = 1")))

(test-write "write-symbol-element (1)"
	    #vu8(#x0E
		 #x61 #x62 #x63 #x00
		 #x04 #x00 #x00 #x00
		 #x61 #x62 #x63 #x00)
	    write-symbol-element '("abc" (symbol "abc")))
(test-write "write-symbol-element (2)"
	    #vu8(#x0E
		 #x61 #x62 #x63 #x00
		 #x04 #x00 #x00 #x00
		 #x61 #x62 #x63 #x00)
	    write-element '("abc" (symbol "abc")))

(test-write "write-javascript/scope-element (1)"
	    #vu8(#x0F
		 #x61 #x62 #x63 #x00
		 #x07 #x00 #x00 #x00
		 #x6e #x20 #x2b #x3d #x20 #x31
		 #x00
		 #x10 #x00 #x00 #x00
		 #x01 #x6E #x00 ;; n
		 #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40 ;; 3.14
		 #x00)
	    write-javascript/scope-element
	    '("abc" (javascript/scope "n += 1" (("n" 3.14)))))
(test-write "write-javascript/scope-element (2)"
	    #vu8(#x0F
		 #x61 #x62 #x63 #x00
		 #x07 #x00 #x00 #x00
		 #x6e #x20 #x2b #x3d #x20 #x31
		 #x00
		 #x10 #x00 #x00 #x00
		 #x01 #x6E #x00 ;; n
		 #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40 ;; 3.14
		 #x00)
	    write-element
	    '("abc" (javascript/scope "n += 1" (("n" 3.14)))))

(test-write "write-int32-element (1)"
	    #vu8(#x10 #x61 #x62 #x63 #x00 #x01 #x00 #x00 #x00)
	    write-int32-element '("abc" (s32 1)))
(test-write "write-int32-element (2)"
	    #vu8(#x10 #x61 #x62 #x63 #x00 #x01 #x00 #x00 #x00)
	    write-element '("abc" (s32 1)))

(test-write "write-uint64-element (1)"
	    #vu8(#x11
		 #x61 #x62 #x63 #x00
		 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
	    write-uint64-element '("abc" (u64 1)))
(test-write "write-uint64-element (2)"
	    #vu8(#x11
		 #x61 #x62 #x63 #x00
		 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
	    write-element '("abc" (u64 1)))

(test-write "write-int64-element (1)"
	    #vu8(#x12
		 #x61 #x62 #x63 #x00
		 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
	    write-int64-element '("abc" (s64 1)))
(test-write "write-int64-element (2)"
	    #vu8(#x12
		 #x61 #x62 #x63 #x00
		 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
	    write-element '("abc" (s64 1)))

(test-write "write-document (1)"
	    #vu8(#x0F #x00 #x00 #x00
		 #xFF #x61 #x62 #x63 #x00
		 #x7F #x61 #x62 #x63 #x00 #x00)
	    write-document '(("abc" min-key) ("abc" max-key)))
(test-end)
