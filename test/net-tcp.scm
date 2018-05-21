(import (rnrs)
	(mongodb net socket)
	(mongodb net tcp)
	(srfi :64))

(test-begin "TCP")

(let ((socket (tcp-connect "google.com" "80")))
  (test-assert (socket? socket))
  (test-assert (input-port? (socket-input-port socket)))
  (test-assert (output-port? (socket-output-port socket)))
  (test-assert (binary-port? (socket-input-port socket)))
  (test-assert (binary-port? (socket-output-port socket)))
  (test-assert (socket-close! socket)))

(test-end)
