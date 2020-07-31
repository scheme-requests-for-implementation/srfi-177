(import (srfi 177))

(define fib
  (lambda (n)
    (if (<= n 2) 1 (+ (fib (- n 2))
                      (fib (- n 1))))))

(define key-fib
  (lambda/kw ((n))
    (if (<= n 2) 1 (+ (call/kw key-fib (n (- n 2)))
                      (call/kw key-fib (n (- n 1)))))))

(time (fib 40))
(time (call/kw key-fib (n 40)))
