(define (split-last lis)
  (let loop ((butlast '()) (lis lis))
    (cond ((null? lis) (values (reverse butlast) #f))
          ((null? (cdr lis)) (values (reverse butlast) (car lis)))
          (else (loop (cons (car lis) butlast) (cdr lis))))))

(define-macro (keyword-lambda formals-and-keys . body)
  (let-values (((formals keyword-symbols) (split-last formals-and-keys)))
    `(lambda (,@formals #!key ,@keyword-symbols)
       ,@body)))

(define-macro (keyword-call kw-lambda . rest)
  (let-values (((positional-vals keyword-syms-and-vals) (split-last rest)))
    (unless (list? keyword-syms-and-vals)
      (error "keyword-call does not end with a list"))
    `(,kw-lambda ,@positional-vals
                 ,@(let loop ((alls keyword-syms-and-vals) (acc '()))
                     (cond ((null? alls) (reverse acc))
                           ((null? (cdr alls))
                            (error "Missing keyword value in keyword-call"))
                           (else (let ((key (string->keyword
                                             (symbol->string (car alls))))
                                       (val (cadr alls)))
                                   (loop (cddr alls)
                                         (cons val (cons key acc))))))))))