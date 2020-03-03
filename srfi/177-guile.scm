(use-modules (ice-9 optargs))

(define-syntax lambda/kw
  (syntax-rules ()
    ((_ (formals ... (keyword-symbols ...)) body ...)
     (lambda* (formals ... #:key keyword-symbols ...) body ...))))

(define-syntax call/kw
  (lambda (stx)
    (syntax-case stx ()
      ((_ kw-lambda positional-vals ... (keyword-syms-and-vals ...))
       #`(kw-lambda
          positional-vals ...
          #,@(let loop ((alls #'(keyword-syms-and-vals ...)) (acc '()))
               (cond ((null? alls) (reverse acc))
                     ((null? (cdr alls))
                      (error "Missing keyword value in call/kw"))
                     (else (let ((key (symbol->keyword
                                       (syntax->datum (car alls))))
                                 (val (cadr alls)))
                             (loop (cddr alls)
                                   (cons val (cons key acc))))))))))))
