#!r6rs
(import (rnrs)
	(mongodb util iso-date)
	(srfi :64))

(test-begin "ISO date util")

(test-equal "2018-05-31T14:25:37.112Z"
	    (milliseconds->iso-date-string 1527776737112))
(test-equal "2018-05-31T00:00:00Z"
	    (milliseconds->iso-date-string 1527724800000))

(test-equal 1527724800000
	    (iso-date-string->milliseconds "2018-05-31"))

(test-end)
