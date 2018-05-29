(import (rnrs)
	(mongodb util uuid)
	(srfi :64))

(test-begin "UUID utilities")

(test-equal "uuid-string->bytevector"
	    #vu8(151 145 159 79 69 33 65 4 146 64 119 81 234 199 218 133)
	    (uuid-string->bytevector "97919f4f-4521-4104-9240-7751eac7da85"))
(test-equal "bytevector->uuid-string"
	    "97919f4f-4521-4104-9240-7751eac7da85"
	    (bytevector->uuid-string
	     #vu8(151 145 159 79 69 33 65 4 146 64 119 81 234 199 218 133)))

(test-end)
