#!r6rs
(library (srfi :98 os-environment-variables)
    (export get-environment-variable
	    get-environment-variables)
    (import (chezscheme))

(define get-environment-variable getenv)
(define (get-environment-variables)
  (raise (condition (make-implementation-restriction-violation)
		    (make-who-condition 'get-environment-variables)
		    (make-message-condition "get-environment-variables is not supported on Chez"))))

)
