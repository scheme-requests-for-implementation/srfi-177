;; Auto-generated
#!r6rs
(library (srfi vector-177)
  (export lambda/kw call/kw)
  (import (rnrs))
  (define (error* x) (error #f x))
  (define-syntax let-false
    (syntax-rules ()
      [(_ () body ...) (begin body ...)]
      [(_ (var) body ...) (let ([var #f]) body ...)]
      [(_ (var vars ...) body ...)
       (let ([var #f]) (let-false (vars ...) body ...))]))
  (define-syntax kw-setters
    (syntax-rules ()
      [(_ sym val)
       (error*
         (if (symbol? sym)
             (string-append "keyword not known: " (symbol->string sym))
             "keyword is not a symbol"))]
      [(_ sym val keyword keywords ...)
       (if (eq? 'keyword sym)
           (set! keyword val)
           (kw-setters sym val keywords ...))]))
  (define-syntax lambda/kw
    (syntax-rules (kvs key val this loop n i)
      [(_ (formals ... (keywords ...)) body ...)
       (letrec ([this (case-lambda
                        [(formals ...) (this formals ... #f)]
                        [(formals ... kvs)
                         (let-false
                           (keywords ...)
                           (when kvs
                             (let ([n (vector-length kvs)])
                               (let loop ([i 0])
                                 (when (< i n)
                                   (let ([sym (vector-ref kvs i)]
                                         [val (vector-ref kvs (+ i 1))])
                                     (kw-setters sym val keywords ...))
                                   (loop (+ i 2))))))
                           ((lambda () body ...)))])])
         this)]))
  (define-syntax kw-call-aux
    (syntax-rules ()
      [(_ kw-lambda (kvs ...) (args ...) ())
       (kw-lambda args ... (vector kvs ...))]
      [(_ kw-lambda (kvs ...) (args ...) (key val more-kvs ...))
       (kw-call-aux
         kw-lambda
         (kvs ... 'key val)
         (args ...)
         (more-kvs ...))]))
  (define-syntax call/kw
    (syntax-rules ()
      [(_ kw-lambda args ... (kvs ...))
       (kw-call-aux kw-lambda () (args ...) (kvs ...))])))
