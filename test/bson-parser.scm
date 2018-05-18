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

(test-values "read-min-key (1)"
	     (4 '(min-key "abc"))
	     (read-bson read-min-key #vu8(#x61 #x62 #x63 #x00)))
(test-values "read-min-key (2)"
	     (5 '(min-key "abc"))
	     (read-bson read-element #vu8(#xFF #x61 #x62 #x63 #x00)))

(test-values "read-max-key (1)"
	     (4 '(max-key "abc"))
	     (read-bson read-max-key #vu8(#x61 #x62 #x63 #x00)))
(test-values "read-max-key (2)"
	     (5 '(max-key "abc"))
	     (read-bson read-element #vu8(#x7F #x61 #x62 #x63 #x00)))

(test-values "read-double (1)"
	     (12 '("abc" 3.14))
	     (read-bson read-double
			#vu8(#x61 #x62 #x63 #x00
			     #x1f #x85 #xeb #x51 #xb8 #x1e #x9 #x40)))
(test-values "read-double (2)"
	     (13 '("abc" 3.14))
	     (read-bson read-element
			#vu8(#x01
			     #x61 #x62 #x63 #x00
			     #x1f #x85 #xeb #x51 #xb8 #x1e #x9 #x40)))

(test-error "read-element (EOF)" bson-error? (read-bson read-element #vu8()))

(test-equal "read-documet (1)" '((min-key "abc"))
	    (read-bson read-document #vu8(#x0A #x00 #x00 #x00
					  #xFF #x61 #x62 #x63 #x00 #x00)))

(test-equal "read-documet (2)" '((max-key "abc"))
	    (read-bson read-document #vu8(#x0A #x00 #x00 #x00
					  #x7F #x61 #x62 #x63 #x00 #x00)))

(test-equal "read-documet (3)" '((min-key "abc") (max-key "abc"))
	    (read-bson read-document #vu8(#x0F #x00 #x00 #x00
					  #xFF #x61 #x62 #x63 #x00
					  #x7F #x61 #x62 #x63 #x00 #x00)))

(test-end)
