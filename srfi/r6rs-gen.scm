(import (chezscheme))

(define (read-all in)
  (let loop ((forms '()))
    (let ((form (read in)))
      (if (eof-object? form) forms (loop (append forms (list form)))))))

(define (variant r5rs r6rs libname)
  (call-with-input-file r5rs
    (lambda (in)
      (let ((forms (read-all in)))
        (call-with-output-file r6rs
          (lambda (out)
            (display ";; Auto-generated\n" out)
            (display "#!r6rs\n" out)
            (pretty-print `(library ,libname
                             (export lambda/kw call/kw)
                             (import (rnrs))
                             (define (error* x) (error #f x))
                             ,@forms)
                          out))
          'replace)))))

(define (main)
  (variant "177-r5rs.scm" "177.sls" '(srfi :177))
  (variant "177-r5rs-vector.scm" "vector-177.sls"' (srfi vector-177)))

(main)
