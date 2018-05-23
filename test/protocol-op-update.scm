#!r6rs
(import (rnrs)
	(mongodb protocol msg-header)
	(mongodb protocol op-update)
	(srfi :64))

(test-begin "OP_UPDATE")

(test-assert (op-update? (make-op-update (make-msg-header))))
(test-error assertion-violation? (make-op-update #f))

(test-end)
