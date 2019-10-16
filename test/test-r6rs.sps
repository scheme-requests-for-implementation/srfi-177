(import (rnrs) (srfi :177))
(define (writeln x) (write x) (newline))
(define x (keyword-lambda (a b (c d)) (list a b c d)))
(writeln (x 1 2))
(writeln (keyword-call x 1 2 ()))
(writeln (keyword-call x 1 2 (c 3)))
(writeln (keyword-call x 1 2 (d 4 c 3)))
(flush-output-port (current-output-port)) ; For Vicare
