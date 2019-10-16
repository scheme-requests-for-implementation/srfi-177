(require compatibility/defmacro)

(define-macro (keyword-lambda formals-and-keys . body)
  (define (split-last lis)
    (let loop ((butlast '()) (lis lis))
      (cond ((null? lis) (values (reverse butlast) #f))
            ((null? (cdr lis)) (values (reverse butlast) (car lis)))
            (else (loop (cons (car lis) butlast) (cdr lis))))))
  (let-values (((formals keyword-symbols) (split-last formals-and-keys)))
    `(lambda (,@formals
              ,@(let loop ((syms keyword-symbols) (acc '()))
                  (if (null? syms) acc
                      (loop (cdr syms)
                            (append acc
                                    `(,(string->keyword
                                        (symbol->string (car syms)))
                                      (,(car syms) #f)))))))
       ,@body)))

(define-macro (keyword-call kw-lambda . rest)
  (define (split-last lis)
    (let loop ((butlast '()) (lis lis))
      (cond ((null? lis) (values (reverse butlast) #f))
            ((null? (cdr lis)) (values (reverse butlast) (car lis)))
            (else (loop (cons (car lis) butlast) (cdr lis))))))
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
