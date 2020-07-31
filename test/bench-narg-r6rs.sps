(import (chezscheme) (srfi :177))

(define (banner x) (newline) (display x) (newline) (newline))

(define pos0 (lambda () #f))
(define key0 (lambda/kw (()) #f))

(define pos1 (lambda (x) x))
(define key1 (lambda/kw ((x)) x))

(define pos2 (lambda (x y) (values x y)))
(define key2 (lambda/kw ((x y)) (values x y)))

(define-syntax bench
  (syntax-rules ()
    ((_ action)
     (begin (time (let loop ((n 100000))
                    (when (> n 0) action (loop (- n 1)))))
            (newline)))))

(banner "Call with 0 arguments")
(bench (pos0))
(bench (call/kw key0 ()))

(banner "Call with 1 argument")
(bench (pos1 #f))
(bench (call/kw key1 (x #f)))

(banner "Call with 2 arguments")
(bench (pos2 #f #f))
(bench (call/kw key2 (x #f y #f)))
