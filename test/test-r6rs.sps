(import (rnrs) (srfi :177))
(define (writeln x) (write x) (newline))
(define x (lambda/kw (a b (c d)) (list a b c d)))
(writeln (x 1 2))
(writeln (call/kw x 1 2 ()))
(writeln (call/kw x 1 2 (c 3)))
(writeln (call/kw x 1 2 (d 4 c 3)))
(flush-output-port (current-output-port)) ; For Vicare
