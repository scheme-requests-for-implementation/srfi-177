(import (chezscheme))

(define (read-all in)
  (let loop ((forms '()))
    (let ((form (read in)))
      (if (eof-object? form) forms (loop (append forms (list form)))))))

(define (main)
  (call-with-input-file "177-r5rs.scm"
    (lambda (in)
      (let ((forms (read-all in)))
        (call-with-output-file "177.sls"
          (lambda (out)
            (display ";; Auto-generated\n" out)
            (display "#!r6rs\n" out)
            (pretty-print `(library (srfi :177)
                             (export keyword-lambda keyword-call)
                             (import (rnrs))
                             (define (error* x) (error #f x))
                             ,@forms)
                          out)))))))

(main)
