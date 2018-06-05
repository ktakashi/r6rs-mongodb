#!r6rs
(import (rnrs)
	(mongodb util iso-date)
	(srfi :64))

(test-begin "ISO date util")

(test-equal "milliseconds->iso-date-string (1)"
	    "2018-05-31T14:25:37.112Z"
	    (milliseconds->iso-date-string 1527776737112))
(test-equal "milliseconds->iso-date-string (2)"
	    "2018-05-31T00:00:00Z"
	    (milliseconds->iso-date-string 1527724800000))

(test-equal "iso-date-string->milliseconds (1)"
	    1527724800000
	    (iso-date-string->milliseconds "2018-05-31"))
(test-equal "iso-date-string->milliseconds (2)"
	    1527776737112
	    (iso-date-string->milliseconds "2018-05-31T14:25:37.112Z"))
(test-equal "iso-date-string->milliseconds (3)"
	    1527776737110
	    (iso-date-string->milliseconds "2018-05-31T14:25:37.11Z"))
(test-equal "iso-date-string->milliseconds (4)"
	    1527776737100
	    (iso-date-string->milliseconds "2018-05-31T14:25:37.1Z"))
(test-equal "iso-date-string->milliseconds (5)"
	    1527776737123
	    (iso-date-string->milliseconds "2018-05-31T14:25:37.123456Z"))
(test-equal "iso-date-string->milliseconds (6)"
	    1527776737000
	    (iso-date-string->milliseconds "2018-05-31T14:25:37"))
(test-equal "iso-date-string->milliseconds (7)"
	    1527776737112
	    (iso-date-string->milliseconds "2018-05-31T14:25:37.112"))

(test-error "iso-date-string->milliseconds (e1)"
	    assertion-violation?
	    (iso-date-string->milliseconds "2018/05/31"))
(test-error "iso-date-string->milliseconds (e2)"
	    assertion-violation?
	    (iso-date-string->milliseconds "201\x0660;-05-31"))
(test-error "iso-date-string->milliseconds (e3)"
	    assertion-violation?
	    (iso-date-string->milliseconds "+201-05-31"))
(test-error "iso-date-string->milliseconds (e4)"
	    assertion-violation?
	    (iso-date-string->milliseconds "2018-05-31T10"))
(test-error "iso-date-string->milliseconds (e5)"
	    assertion-violation?
	    (iso-date-string->milliseconds "2018-05-31T"))
(test-error "iso-date-string->milliseconds (e6)"
	    assertion-violation?
	    (iso-date-string->milliseconds "2018-05-31T10:10"))
(test-error "iso-date-string->milliseconds (e7)"
	    assertion-violation?
	    (iso-date-string->milliseconds "2018-05-31T10:10:10.Z"))
(test-error "iso-date-string->milliseconds (e8)"
	    assertion-violation?
	    (iso-date-string->milliseconds "2018-05-31T10:10:10.1aZ"))
(test-error "iso-date-string->milliseconds (e9)"
	    assertion-violation?
	    (iso-date-string->milliseconds "2018-05-31T10:10:10.1A"))

(test-end)
