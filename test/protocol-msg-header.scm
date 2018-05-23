#!r6rs
(import (rnrs)
	(mongodb protocol msg-header)
	(srfi :64))

(test-begin "MsgHeader")

(test-assert (msg-header? (make-msg-header 0 0 0 0)))
(let ((msg-header (make-msg-header 1 2 3 4)))
  (test-equal 1 (msg-header-message-length msg-header))
  (test-equal 2 (msg-header-request-id msg-header))
  (test-equal 3 (msg-header-response-to msg-header))
  (test-equal 4 (msg-header-op-code msg-header)))

(let ((r (make-msg-header))
      (in (open-bytevector-input-port
	   (sint-list->bytevector '(1 2 3 4) (endianness little) 4))))
  (test-assert (msg-header? (read-msg-header! in r)))
  (test-equal "read-msg-header! (len)" 1 (msg-header-message-length r))
  (test-equal "read-msg-header! (id)" 2 (msg-header-request-id r))
  (test-equal "read-msg-header! (to)" 3 (msg-header-response-to r))
  (test-equal "read-msg-header! (op)" 4 (msg-header-op-code r)))

(let ((in (open-bytevector-input-port
	   (sint-list->bytevector '(1 2 3 4) (endianness little) 4))))
  (let ((r (read-msg-header in)))
    (test-assert (msg-header? r))
    (test-equal "read-msg-header! (len)" 1 (msg-header-message-length r))
    (test-equal "read-msg-header! (id)" 2 (msg-header-request-id r))
    (test-equal "read-msg-header! (to)" 3 (msg-header-response-to r))
    (test-equal "read-msg-header! (op)" 4 (msg-header-op-code r))))

(test-end)
