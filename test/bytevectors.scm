(import (rnrs)
	(mongodb util bytevectors)
	(srfi :64))

(test-begin "Bytevector Utils")

(test-equal "bytevector->hex-string" "12345678" 
	    (bytevector->hex-string #vu8(#x12 #x34 #x56 #x78)))
(test-equal "bytevector->hex-string" "0abc" 
	    (bytevector->hex-string #vu8(#x0A #xBC)))
(test-equal "bytevector->hex-string" "0ABC" 
	    (bytevector->hex-string #vu8(#x0A #xBC) #t))


(test-end)
