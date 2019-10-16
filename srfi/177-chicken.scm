(import-for-syntax (chicken keyword))

(define (split-last lis)
  (let loop ((butlast '()) (lis lis))
    (cond ((null? lis) (values (reverse butlast) #f))
          ((null? (cdr lis)) (values (reverse butlast) (car lis)))
          (else (loop (cons (car lis) butlast) (cdr lis))))))

(define-syntax keyword-lambda
  (syntax-rules ()
    ((_ (formals ... (keywords ...)) body ...)
     (lambda (formals ... #!key keywords ...) body ...))))

(define-syntax keyword-call
  (er-macro-transformer
   (lambda (exp rename compare)
     (let ((kw-lambda (cadr exp)))
       (let-values (((positional-vals keyword-syms-and-vals)
                     (split-last (cddr exp))))
         (unless (list? (rename keyword-syms-and-vals))
           (error "keyword-call does not end with a list"))
         `(,kw-lambda
           ,@positional-vals
           ,@(let loop ((alls keyword-syms-and-vals) (acc '()))
               (cond ((null? alls) (reverse acc))
                     ((null? (cdr alls))
                      (error "Missing keyword value in keyword-call"))
                     (else (let ((key (string->keyword
                                       (symbol->string (car alls))))
                                 (val (cadr alls)))
                             (loop (cddr alls)
                                   (cons val (cons key acc)))))))))))))
