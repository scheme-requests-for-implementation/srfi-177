;; This file is to be imported from the R7RS modes of these Schemes.
;; For the native, non-R7RS mode, (load "*.scm") instead.

(define-library (srfi 177)
  (export lambda/kw call/kw)
  (import (scheme base))
  (cond-expand
    (chicken
     (include "177-chicken.scm"))
    (gambit
     (import (gambit))
     (include "177-gambit.scm"))
    (gauche
     (import (gauche base) (srfi 8))
     (include "177-ga-sa-st.scm"))
    (kawa
     (import (kawa base))
     (include "177-kawa.scm"))
    (sagittarius
     (import (sagittarius))
     (include "177-ga-sa-st.scm"))
    (r7rs
     (import (scheme base))
     (begin (define error* error))
     (include "177-r5rs.scm"))))
