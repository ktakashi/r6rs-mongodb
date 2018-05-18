#!r6rs
(import (rnrs)
	(binary bson parser)
	(binary bson conditions)
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
	     (4 '(min-key "abc"))
	     (read-bson read-min-key-element #vu8(#x61 #x62 #x63 #x00)))
(test-values "read-min-key-element (2)"
	     (5 '(min-key "abc"))
	     (read-bson read-element #vu8(#xFF #x61 #x62 #x63 #x00)))

(test-values "read-max-key-element (1)"
	     (4 '(max-key "abc"))
	     (read-bson read-max-key-element #vu8(#x61 #x62 #x63 #x00)))
(test-values "read-max-key-element (2)"
	     (5 '(max-key "abc"))
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
	     (19 '("abc" ((min-key "abc") (max-key "abc"))))
	     (read-bson read-embedded-document-element
			#vu8(#x61 #x62 #x63 #x00
			     #x0F #x00 #x00 #x00
			     #xFF #x61 #x62 #x63 #x00
			     #x7F #x61 #x62 #x63 #x00 #x00)))
(test-values "read-embedded-document-element (1)"
	     (20 '("abc" ((min-key "abc") (max-key "abc"))))
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
(test-values "read-binary-element (1)"
	     (14 '("abc" (binary #x00 #vu8(1 2 3 4))))
	     (read-bson read-element
			#vu8(#x05
			     #x61 #x62 #x63 #x00
			     #x04 #x00 #x00 #x00
			     #x00
			     #x01 #x02 #x03 #x04)))

(test-error "read-element (EOF)" bson-error? (read-bson read-element #vu8()))

(test-values "read-documet (1)"
	     (#x0A '((min-key "abc")))
	     (read-bson read-document #vu8(#x0A #x00 #x00 #x00
					   #xFF #x61 #x62 #x63 #x00 #x00)))

(test-values "read-documet (2)"
	    (#x0A '((max-key "abc")))
	    (read-bson read-document #vu8(#x0A #x00 #x00 #x00
					  #x7F #x61 #x62 #x63 #x00 #x00)))

(test-values "read-documet (3)"
	    (#x0F '((min-key "abc") (max-key "abc")))
	    (read-bson read-document #vu8(#x0F #x00 #x00 #x00
					  #xFF #x61 #x62 #x63 #x00
					  #x7F #x61 #x62 #x63 #x00 #x00)))

(test-end)
