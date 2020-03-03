(define-syntax lambda/kw
  (lambda (stx)
    (syntax-case stx ()
      ((_ (formals ... (keyword-symbols ...)) body ...)
       #`(lambda
             (formals
              ...
              #,@(let loop ((syms (syntax->datum #'(keyword-symbols ...)))
                            (acc '()))
                   (if (null? syms) acc
                       (loop (cdr syms)
                             (let ((key (string->keyword
                                         (symbol->string (car syms))))
                                   (sym (datum->syntax stx (car syms))))
                               (append acc `(,key (,sym #f))))))))
           body ...)))))

(define-syntax call/kw
  (lambda (stx)
    (syntax-case stx ()
      ((_ kw-lambda positional-vals ... (keyword-syms-and-vals ...))
       #`(kw-lambda
          positional-vals ...
          #,@(let loop ((alls (syntax->datum #'(keyword-syms-and-vals ...)))
                        (acc '()))
               (cond ((null? alls) (datum->syntax stx (reverse acc)))
                     ((null? (cdr alls))
                      (error "Missing keyword value in call/kw"))
                     (else (let ((key (string->keyword
                                       (symbol->string (car alls))))
                                 (val (cadr alls)))
                             (loop (cddr alls)
                                   (cons val (cons key acc))))))))))))
