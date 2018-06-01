#!r6rs
(import (rnrs)
	(mongodb bson parser)
	(mongodb bson conditions)
	(mongodb util parameters)
	(srfi :64))

(test-begin "BSON parser")

(define-syntax test-values
  (syntax-rules ()
    ((_ "collect" name (ex ex* ...) ((e v) ...) expr)
     (test-values "collect" name (ex* ...) ((e v) ... (ex tmp)) expr))
    ((_ "collect" name () ((expected var) ...) expr)
     (let-values (((var ...) expr))
       (test-equal '(name expected) expected var) ...))
    ((_ name (expected ...) expr)
     (test-values "collect" name (expected ...) () expr))
    ((_ (expected ...) expr)
     (test-values 'expr (expected ...) expr))))

(define (read-bson reader bv)
  (reader (open-bytevector-input-port bv)))

(test-error "read-cstring (EOF)" bson-error? (read-bson read-cstring #vu8()))
(test-error "read-cstring (EOF)" bson-error?
	    (read-bson read-cstring #vu8(#x61)))

(test-values "read-cstring (1)"
	     (4 "abc")
	     (read-bson read-cstring #vu8(#x61 #x62 #x63 #x00)))
(test-values "read-string (1)"
	     (8 "abc")
	     (read-bson read-string #vu8(#x04 #x00 #x00 #x00
					 #x61 #x62 #x63 #x00)))

(test-error "read-binary (EOF)" bson-error?
	     (read-bson read-binary #vu8(#x04 #x00 #x00 #x00)))
(test-values "read-binary (1)"
	     (9 '(binary #x00 #vu8(1 2 3 4)))
	     (read-bson read-binary #vu8(#x04 #x00 #x00 #x00
					 #x00
					 #x01 #x02 #x03 #x04)))

(test-values "read-min-key-element (1)"
	     (4 '("abc" min-key))
	     (read-bson read-min-key-element #vu8(#x61 #x62 #x63 #x00)))
(test-values "read-min-key-element (2)"
	     (5 '("abc" min-key))
	     (read-bson read-element #vu8(#xFF #x61 #x62 #x63 #x00)))

(test-values "read-max-key-element (1)"
	     (4 '("abc" max-key))
	     (read-bson read-max-key-element #vu8(#x61 #x62 #x63 #x00)))
(test-values "read-max-key-element (2)"
	     (5 '("abc" max-key))
	     (read-bson read-element #vu8(#x7F #x61 #x62 #x63 #x00)))

(test-values "read-double-element (1)"
	     (12 '("abc" 3.14))
	     (read-bson read-double-element
			#vu8(#x61 #x62 #x63 #x00
			     #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)))
(test-values "read-double-element (2)"
	     (13 '("abc" 3.14))
	     (read-bson read-element
			#vu8(#x01
			     #x61 #x62 #x63 #x00
			     #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)))
(test-values "read-double-element (3)"
	     (13 '("abc" 1))
	     (read-bson read-element
			#vu8(#x01
			     #x61 #x62 #x63 #x00
			     #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x3f)))

(test-values "read-string-element (1)"
	     (12 '("abc" "abc"))
	     (read-bson read-string-element
			#vu8(#x61 #x62 #x63 #x00
			     #x04 #x00 #x00 #x00 #x61 #x62 #x63 #x00)))
(test-values "read-string-element (2)"
	     (13 '("abc" "abc"))
	     (read-bson read-element
			#vu8(#x02
			     #x61 #x62 #x63 #x00
			     #x04 #x00 #x00 #x00 #x61 #x62 #x63 #x00)))

(test-values "read-embedded-document-element (1)"
	     (19 '("abc" (("abc" min-key) ("abc" max-key))))
	     (read-bson read-embedded-document-element
			#vu8(#x61 #x62 #x63 #x00
			     #x0F #x00 #x00 #x00
			     #xFF #x61 #x62 #x63 #x00
			     #x7F #x61 #x62 #x63 #x00 #x00)))
(test-values "read-embedded-document-element (1)"
	     (20 '("abc" (("abc" min-key) ("abc" max-key))))
	     (read-bson read-element
			#vu8(#x03
			     #x61 #x62 #x63 #x00
			     #x0F #x00 #x00 #x00
			     #xFF #x61 #x62 #x63 #x00
			     #x7F #x61 #x62 #x63 #x00 #x00)))

(test-values "read-array-element (1)"
	     (31 '("abc" #(3.14 "abc")))
	     (read-bson read-array-element
			#vu8(#x61 #x62 #x63 #x00
			     #x1B #x00 #x00 #x00
			     #x01 #x30 #x00 ;; "0"
			     #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40
			     #x02 #x31 #x00 ;; "1"
			     #x04 #x00 #x00 #x00 #x61 #x62 #x63 #x00
			     #x00)))
(test-values "read-array-element (2)"
	     (32 '("abc" #(3.14 "abc")))
	     (read-bson read-element
			#vu8(#x04
			     #x61 #x62 #x63 #x00
			     #x1B #x00 #x00 #x00
			     #x01 #x30 #x00 ;; "0"
			     #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40
			     #x02 #x31 #x00 ;; "1"
			     #x04 #x00 #x00 #x00 #x61 #x62 #x63 #x00
			     #x00)))
;; not sure if this is an error though...
(test-error "read-array-element (3)" bson-error?
	     (read-bson read-element
			#vu8(#x04
			     #x61 #x62 #x63 #x00
			     #x1B #x00 #x00 #x00
			     #x01 #x30 #x00 ;; "0"
			     #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40
			     #x02 #x32 #x00 ;; "2"
			     #x04 #x00 #x00 #x00 #x61 #x62 #x63 #x00
			     #x00)))
(test-error "read-array-element (4)" bson-error?
	     (read-bson read-element
			#vu8(#x04
			     #x61 #x62 #x63 #x00
			     #x1B #x00 #x00 #x00
			     #x01 #x31 #x00 ;; "1"
			     #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40
			     #x02 #x30 #x00 ;; "0"
			     #x04 #x00 #x00 #x00 #x61 #x62 #x63 #x00
			     #x00)))

(test-values "read-binary-element (1)"
	     (13 '("abc" (binary #x00 #vu8(1 2 3 4))))
	     (read-bson read-binary-element
			#vu8(#x61 #x62 #x63 #x00
			     #x04 #x00 #x00 #x00
			     #x00
			     #x01 #x02 #x03 #x04)))
(test-values "read-binary-element (2)"
	     (14 '("abc" (binary #x00 #vu8(1 2 3 4))))
	     (read-bson read-element
			#vu8(#x05
			     #x61 #x62 #x63 #x00
			     #x04 #x00 #x00 #x00
			     #x00
			     #x01 #x02 #x03 #x04)))

(test-values "read-binary-element (3)"
	     (26 '("abc" (uuid "97919f4f-4521-4104-9240-7751eac7da85")))
	     (read-bson read-element
			#vu8(#x05
			     #x61 #x62 #x63 #x00
			     #x10 #x00 #x00 #x00
			     #x04
			     151 145 159 79 69 33 65 4 146
			     64 119 81 234 199 218 133)))

(test-values "read-undefined-element (1)"
	     (4 '("abc" undefined))
	     (read-bson read-undefined-element #vu8(#x61 #x62 #x63 #x00)))
(test-values "read-undefined-element (2)"
	     (5 '("abc" undefined))
	     (read-bson read-element #vu8(#x06 #x61 #x62 #x63 #x00)))

(test-values "read-object-id-element (1)"
	     (16 '("abc" (object-id "5afec5ca859a71dfd75da8d0")))
	     (read-bson read-object-id-element
			#vu8(#x61 #x62 #x63 #x00
			     #x5a #xfe #xc5 #xca
			     #x85 #x9a #x71 #xdf
			     #xd7 #x5d #xa8 #xd0)))

(test-values "read-object-id-element (2)"
	     (17 '("abc" (object-id "5afec5ca859a71dfd75da8d0")))
	     (read-bson read-element
			#vu8(#x07
			     #x61 #x62 #x63 #x00
			     #x5a #xfe #xc5 #xca
			     #x85 #x9a #x71 #xdf
			     #xd7 #x5d #xa8 #xd0)))

(test-values "read-boolean-element (1)"
	     (5 '("abc" #f))
	     (read-bson read-boolean-element #vu8(#x61 #x62 #x63 #x00 #x00)))
(test-values "read-boolean-element (2)"
	     (5 '("abc" #t))
	     (read-bson read-boolean-element #vu8(#x61 #x62 #x63 #x00 #x01)))
(test-error "read-boolean-element (3)" bson-error?
	     (read-bson read-boolean-element #vu8(#x61 #x62 #x63 #x00 #x03)))
(test-error "read-boolean-element (4)" bson-error?
	     (read-bson read-boolean-element #vu8(#x61 #x62 #x63 #x00)))

(test-values "read-boolean-element (5)"
	     (6 '("abc" #t))
	     (read-bson read-element #vu8(#x08 #x61 #x62 #x63 #x00 #x01)))

(test-values "read-utc-datetime-element (1)"
	     (12 '("abc" (utc-datetime 1)))
	     (read-bson read-utc-datetime-element
			#vu8(#x61 #x62 #x63 #x00
			     #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))
(test-values "read-utc-datetime-element (2)"
	     (13 '("abc" (utc-datetime 1)))
	     (read-bson read-element
			#vu8(#x09
			     #x61 #x62 #x63 #x00
			     #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))
(parameterize ((*bson:use-iso-date?* #t))
  (test-values "read-utc-datetime-element (3)"
	       (13 '("abc" (iso-date "1970-01-01T00:00:00Z")))
	       (read-bson read-element
			  #vu8(#x09
			       #x61 #x62 #x63 #x00
			       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))))


(test-values "read-null-element (1)"
	     (4 '("abc" null))
	     (read-bson read-null-element #vu8(#x61 #x62 #x63 #x00)))
(test-values "read-null-element (2)"
	     (5 '("abc" null))
	     (read-bson read-element #vu8(#x0A #x61 #x62 #x63 #x00)))

(test-values "read-regex-element (1)"
	     (9 '("abc" (regex "\\w" "i")))
	     (read-bson read-regex-element
			#vu8(#x61 #x62 #x63 #x00
			     #x5C #x77 #x00 ;; \w
			     #x69 #x00      ;; i
			     )))
(test-values "read-regex-element (2)"
	     (8 '("abc" (regex "\\w" "")))
	     (read-bson read-regex-element
			#vu8(#x61 #x62 #x63 #x00
			     #x5C #x77 #x00 ;; \w
			     #x00      ;; ""
			     )))
(test-values "read-regex-element (3)"
	     (10 '("abc" (regex "\\w" "il")))
	     (read-bson read-regex-element
			#vu8(#x61 #x62 #x63 #x00
			     #x5C #x77 #x00 ;; \w
			     #x69 #x6C #x00 ;; il
			     )))
(test-error "read-regex-element (4)" bson-error?
	     (read-bson read-regex-element
			#vu8(#x61 #x62 #x63 #x00
			     #x5C #x77 #x00 ;; \w
			     #x71 #x00      ;; q
			     )))
(test-error "read-regex-element (5)" bson-error?
	     (read-bson read-regex-element
			#vu8(#x61 #x62 #x63 #x00
			     #x5C #x77 #x00 ;; \w
			     #x6c #x71 #x00 ;; li
			     )))

(test-values "read-regex-element (6)"
	     (11 '("abc" (regex "\\w" "il")))
	     (read-bson read-element
			#vu8(#x0B
			     #x61 #x62 #x63 #x00
			     #x5C #x77 #x00 ;; \w
			     #x69 #x6C #x00 ;; il
			     )))

(test-values "read-db-pointer-element (1)"
	     (22 '("abc" (db-pointer "p" #vu8(1 2 3 4 5 6 7 8 9 10 11 12))))
	     (read-bson read-db-pointer-element
			#vu8(#x61 #x62 #x63 #x00
			     #x02 #x00 #x00 #x00 #x70 #x00 ;; p
			     1 2 3 4 5 6 7 8 9 10 11 12)))

(test-values "read-db-pointer-element (2)"
	     (23 '("abc" (db-pointer "p" #vu8(1 2 3 4 5 6 7 8 9 10 11 12))))
	     (read-bson read-element
			#vu8(#x0C
			     #x61 #x62 #x63 #x00
			     #x02 #x00 #x00 #x00 #x70 #x00 ;; p
			     1 2 3 4 5 6 7 8 9 10 11 12)))

(test-values "read-javascript-element (1)"
	     (18 '("abc" (javascript "var n = 1")))
	     (read-bson read-javascript-element
			#vu8(#x61 #x62 #x63 #x00
			     #x0A #x00 #x00 #x00 
			     #x76 #x61 #x72 #x20 #x6e #x20 #x3d #x20 #x31
			     #x00 
			     )))
(test-values "read-javascript-element (2)"
	     (19 '("abc" (javascript "var n = 1")))
	     (read-bson read-element
			#vu8(#x0D
			     #x61 #x62 #x63 #x00
			     #x0A #x00 #x00 #x00 
			     #x76 #x61 #x72 #x20 #x6e #x20 #x3d #x20 #x31
			     #x00 
			     )))

(test-values "read-symbol-element (1)"
	     (12 '("abc" (symbol "abc")))
	     (read-bson read-symbol-element
			#vu8(#x61 #x62 #x63 #x00
			     #x04 #x00 #x00 #x00
			     #x61 #x62 #x63 #x00)))
(test-values "read-symbol-element (2)"
	     (13 '("abc" (symbol "abc")))
	     (read-bson read-element
			#vu8(#x0E
			     #x61 #x62 #x63 #x00
			     #x04 #x00 #x00 #x00
			     #x61 #x62 #x63 #x00)))

(test-values "read-javascript/scope-element (1)"
	     (31 '("abc" (javascript/scope "n += 1" (("n" 3.14)))))
	     (read-bson read-javascript/scope-element
			#vu8(#x61 #x62 #x63 #x00
			     #x07 #x00 #x00 #x00
			     #x6e #x20 #x2b #x3d #x20 #x31
			     #x00
			     #x10 #x00 #x00 #x00
			     #x01 #x6E #x00 ;; n
			     #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40 ;; 3.14
			     #x00
			     )))
(test-values "read-javascript/scope-element (1)"
	     (32 '("abc" (javascript/scope "n += 1" (("n" 3.14)))))
	     (read-bson read-element
			#vu8(#x0F
			     #x61 #x62 #x63 #x00
			     #x07 #x00 #x00 #x00
			     #x6e #x20 #x2b #x3d #x20 #x31
			     #x00
			     #x10 #x00 #x00 #x00
			     #x01 #x6E #x00 ;; n
			     #x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40 ;; 3.14
			     #x00
			     )))

(test-values "read-int32-element (1)"
	     (8 '("abc" (s32 1)))
	     (read-bson read-int32-element
			#vu8(#x61 #x62 #x63 #x00 #x01 #x00 #x00 #x00)))
(test-values "read-int32-element (2)"
	     (9 '("abc" (s32 1)))
	     (read-bson read-element
			#vu8(#x10 #x61 #x62 #x63 #x00 #x01 #x00 #x00 #x00)))

(test-values "read-uint64-element (1)"
	     (12 '("abc" (u64 1)))
	     (read-bson read-uint64-element
			#vu8(#x61 #x62 #x63 #x00
			     #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))
(test-values "read-uint64-element (2)"
	     (13 '("abc" (u64 1)))
	     (read-bson read-element
			#vu8(#x11
			     #x61 #x62 #x63 #x00
			     #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))

(test-values "read-int64-element (1)"
	     (12 '("abc" (s64 1)))
	     (read-bson read-int64-element
			#vu8(#x61 #x62 #x63 #x00
			     #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))
(test-values "read-int64-element (2)"
	     (13 '("abc" (s64 1)))
	     (read-bson read-element
			#vu8(#x12
			     #x61 #x62 #x63 #x00
			     #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))

;; decimal128 is not supported (hopefully yet). so no test

(test-error "read-element (EOF)" bson-error? (read-bson read-element #vu8()))

(test-values "read-documet (1)"
	     (#x0A '(("abc" min-key)))
	     (read-bson read-document #vu8(#x0A #x00 #x00 #x00
					   #xFF #x61 #x62 #x63 #x00 #x00)))

(test-values "read-documet (2)"
	    (#x0A '(("abc" max-key)))
	    (read-bson read-document #vu8(#x0A #x00 #x00 #x00
					  #x7F #x61 #x62 #x63 #x00 #x00)))

(test-values "read-documet (3)"
	    (#x0F '(("abc" min-key) ("abc" max-key)))
	    (read-bson read-document #vu8(#x0F #x00 #x00 #x00
					  #xFF #x61 #x62 #x63 #x00
					  #x7F #x61 #x62 #x63 #x00 #x00)))
(test-values "read-documet (4)"
	     (#x05 '())
	     (read-bson read-document #vu8(#x05 #x00 #x00 #x00 #x00)))

(test-end)
