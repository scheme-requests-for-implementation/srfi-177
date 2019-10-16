(define-syntax keyword-lambda
  (syntax-rules ()
    ((_ (formals ... (keyword-symbols ...)) body ...)
     (lambda (formals ... #!key keyword-symbols ...) body ...))))

(define-syntax-case keyword-call ()
  ((_ kw-lambda positional-vals ... (keyword-syms-and-vals ...))
   #`(kw-lambda positional-vals ...
                #,@(let loop ((alls #'(keyword-syms-and-vals ...)) (acc '()))
                     (cond ((null? alls) (reverse acc))
                           ((null? (cdr alls))
                            (error "Missing keyword value in keyword-call"))
                           (else (let ((key (string->keyword
                                             (symbol->string (car alls))))
                                       (val (cadr alls)))
                                   (loop (cddr alls)
                                         (cons val (cons key acc))))))))))
