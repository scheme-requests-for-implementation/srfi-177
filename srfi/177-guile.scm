(use-modules (ice-9 optargs))

(define-syntax keyword-lambda
  (syntax-rules ()
    ((_ (formals ... (keyword-symbols ...)) body ...)
     (lambda* (formals ... #:key keyword-symbols ...) body ...))))

(define-syntax keyword-call
  (lambda (stx)
    (syntax-case stx ()
      ((_ kw-lambda positional-vals ... (keyword-syms-and-vals ...))
       #`(kw-lambda
          positional-vals ...
          #,@(let loop ((alls #'(keyword-syms-and-vals ...)) (acc '()))
               (cond ((null? alls) (reverse acc))
                     ((null? (cdr alls))
                      (error "Missing keyword value in keyword-call"))
                     (else (let ((key (symbol->keyword
                                       (syntax->datum (car alls))))
                                 (val (cadr alls)))
                             (loop (cddr alls)
                                   (cons val (cons key acc))))))))))))
