;;
;; Compile-time expansion of keyword arguments
;;
;; A keyword-arugment-taking procedure defined by suggested srfi-177 syntax:
;;
;;   (define/kw (proc arg ... (keyarg ...)) body ...)
;;
;; will be translated two procedures; first one is a core procedure:
;;
;;   (define ($real-proc arg ... keyarg ...) body ...)
;;
;; in which $real-proc is actually a renamed identifier hidden from outside.
;; The second one is a wrapper procedure:
;;
;;   (define (proc arg ... :key (keyarg #f) ...)
;;     ($real-proc arg ... keyarg ...))
;;
;; which is defined as a hybrid syntax (Gauche's way of compiler macro),
;; and if it is invoked as a procedure call:
;;
;;   (proc arg ... :keyarg val ...)
;;
;; then keyword parsing is done at a compile-time, and becomes a simple
;; invocation of $real-proc.
;;
;; At expansion time, keywords are compared hygienically with gauche.keyword
;; module.  If the call site gives keywords in other ways, we fall back to
;; the runtime parsing based on the value.  Every keyword in gauche.keyword
;; module are constantly bound to itself, so compile-time and run-time
;; behavior always match.

(use gauche.sequence)
(use util.match)

;; substitute k-th element of lis with s
(define (subst lis k s)
  (append (subseq lis 0 k) (list s) (subseq lis (+ k 1))))

;; (define/kw (proc arg ... (keyarg ...)) body ...)
(define-syntax define/kw
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (proc arg args ...) body ...)
        (let ([ord-args (drop-right (cons arg args) 1)]
              [kw-args (last args)]
              [ff (r'ff)]
              [rr (r'rr)]
              [cc (r'cc)]
              [loop (r'loop)]
              [kvs (r'kvs)])
          (quasirename r
            `(begin
               (define (real-proc ,@ord-args ,@kw-args)
                 ,@body)
               (define-hybrid-syntax ,proc
                 (lambda (,@ord-args :key ,@(map (^k `(,k #f)) kw-args))
                   (real-proc ,@ord-args ,@kw-args))
                 (er-macro-transformer
                  (^[,ff ,rr ,cc]
                    (match ,ff
                      [(_ ,@ord-args . ,kvs)
                       (let ,loop ([,kvs ,kvs] ,@(map (^k `(,k #f)) kw-args))
                         (cond [(null? ,kvs)
                                (quasirename rr
                                  `(real-proc ,,@ord-args ,,@kw-args))]
                               ,@(map-with-index
                                  (^[i k]
                                    `([,cc (,rr ,(make-keyword k)) (car ,kvs)]
                                      (if (null? (cdr ,kvs))
                                        ,ff
                                        (,loop (cddr ,kvs)
                                               ,@(subst kw-args i
                                                        `(cadr ,kvs))))))
                                  kw-args)
                               [else ,ff]))]
                      [_ ,ff])))))))]))))

#|
(define/kw (foo a b (c d e)) (list a b c d e))
  =>

(begin
 (define (real-proc.0 a b c d e) (list a b c d e))
 (define-hybrid-syntax
  foo
  (lambda (a b :key (c #f) (d #f) (e #f)) (real-proc.0 a b c d e))
  (er-macro-transformer
   (^
    (ff.0 rr.0 cc.0)
    (match
     ff.0
     ((_ a b . kvs.0)
      (let
       loop.0
       ((kvs.0 kvs.0) (c #f) (d #f) (e #f))
       (cond
        ((null? kvs.0)
         (quasirename rr.0 `(real-proc.0 (unquote a b) (unquote c d e))))
        ((cc.0 (rr.0 :c) (car kvs.0))
         (if (null? (cdr kvs.0)) ff.0 (loop.0 (cddr kvs.0) (cadr kvs.0) d e)))
        ((cc.0 (rr.0 :d) (car kvs.0))
         (if (null? (cdr kvs.0)) ff.0 (loop.0 (cddr kvs.0) c (cadr kvs.0) e)))
        ((cc.0 (rr.0 :e) (car kvs.0))
         (if (null? (cdr kvs.0)) ff.0 (loop.0 (cddr kvs.0) c d (cadr kvs.0))))
        (else ff.0))))
     (_ ff.0))))))
|#
