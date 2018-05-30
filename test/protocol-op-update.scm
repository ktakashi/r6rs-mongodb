#!r6rs
(import (rnrs)
	(mongodb protocol msg-header)
	(mongodb protocol op-update)
	(srfi :64))

(test-begin "OP_UPDATE")

(test-assert (op-update? (make-op-update "test" 0 '() '())))
(test-error assertion-violation? (make-op-update #f 0 0 0 '() '()))
(test-error assertion-violation?
	    (make-op-update (make-msg-header) 0 0 0 '() '()))

(let ((msg (make-op-update "test" 0 '() '())))
  (define raw-msg #vu8(0 0 0 0
		       #x61 #x62 #x63 #x00
		       0 0 0 0
		       #x05 #x00 #x00 #x00 #x00
		       #x05 #x00 #x00 #x00 #x00))
  (let ((in (open-bytevector-input-port raw-msg)))
    (test-error "Invalid message size"
		assertion-violation? (read-op-update! in msg)))
  (let ((in (open-bytevector-input-port raw-msg)))
    (msg-header-message-length-set! (mongodb-protocol-message-header msg)
				    (+ (bytevector-length raw-msg) *msg-header-size*))
    (test-assert "read-op-update!" (op-update? (read-op-update! in msg)))))

(test-end)
