;; Requires tail patterns from SRFI 46.

(define-syntax let-false
  (syntax-rules ()
    ((_ () body ...) (begin body ...))
    ((_ (var) body ...) (let ((var #f)) body ...))
    ((_ (var vars ...) body ...)
     (let ((var #f)) (let-false (vars ...) body ...)))))

(define-syntax kw-setters
  (syntax-rules ()
    ((_ sym val)
     (error* (if (symbol? sym)
                 (string-append "keyword not known: " (symbol->string sym))
                 "keyword is not a symbol")))
    ((_ sym val keyword keywords ...)
     (if (eq? 'keyword sym)
         (set! keyword val)
         (kw-setters sym val keywords ...)))))

(define-syntax lambda/kw
  (syntax-rules (kv val vals loop i)
    ((_ (formals ... (keywords ...)) body ...)
     (lambda (formals ... . kv)
       (let-false (keywords ...)
         (let loop ((kv kv))
           (cond ((null? kv))
                 ((not (pair? (cdr kv)))
                  (error* "Keyword without value"))
                 (else
                  (let ((sym (car kv)) (val (cadr kv)))
                    (kw-setters sym val keywords ...))
                  (loop (cddr kv)))))
         ((lambda () body ...)))))))

;;

(define-syntax kw-call-aux
  (syntax-rules ()
    ((_ kw-lambda (kvs ...) (args ...) ())
     (kw-lambda args ... kvs ...))
    ((_ kw-lambda (kvs ...) (args ...) (key val more-kvs ...))
     (kw-call-aux kw-lambda (kvs ... 'key val) (args ...) (more-kvs ...)))))

(define-syntax call/kw
  (syntax-rules ()
    ((_ kw-lambda args ... (kvs ...))
     (kw-call-aux kw-lambda () (args ...) (kvs ...)))))
