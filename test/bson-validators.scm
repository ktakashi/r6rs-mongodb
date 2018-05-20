(import (rnrs)
	(mongodb bson validators)
	(mongodb bson conditions)
	(srfi :64))

(test-begin "BSON validators")

(test-equal "check-regex-flag-order (1)" "i" (check-regex-flag-order #f "i"))
(test-equal "check-regex-flag-order (2)" "il" (check-regex-flag-order #f "il"))
(test-equal "check-regex-flag-order (3)" "" (check-regex-flag-order #f ""))
(test-error "check-regex-flag-order (4)" bson-error?
	    (check-regex-flag-order #f "f"))

(test-end)
