(import (scheme base) (scheme time) (scheme write) (srfi 177))

(define (banner x) (newline) (display x) (newline) (newline))

(define pos0 (lambda () #f))
(define key0 (lambda/kw (()) #f))

(define pos1 (lambda (x) x))
(define key1 (lambda/kw ((x)) x))

(define pos2 (lambda (x y) (values x y)))
(define key2 (lambda/kw ((x y)) (values x y)))

(define pos3 (lambda (x y z) (values x y z)))
(define key3 (lambda/kw ((x y z)) (values x y z)))

(define-syntax bench
  (syntax-rules ()
    ((_ n-times action)
     (let ((start (current-jiffy)))
       (let loop ((n n-times))
         (when (> n 0) action (loop (- n 1))))
       (let ((duration (/ (- (current-jiffy) start)
                          (jiffies-per-second))))
         (display (inexact duration))
         (newline))))))

(banner "Call with 0 arguments")
(bench 100000 (pos0))
(bench 100000 (call/kw key0 ()))

(banner "Call with 1 argument")
(bench 100000 (pos1 #f))
(bench 100000 (call/kw key1 (x #f)))

(banner "Call with 2 arguments")
(bench 100000 (pos2 #f #f))
(bench 100000 (call/kw key2 (x #f y #f)))

(banner "Call with 3 arguments")
(bench 100000 (pos3 #f #f #f))
(bench 100000 (call/kw key3 (x #f y #f z #f)))

(define mr-bigglesworth
  (lambda/kw
   (x y z
      (a0 b0 c0 d0 e0 f0 g0 h0 i0 j0 k0 l0 m0
          n0 o0 p0 q0 r0 s0 t0 u0 v0 w0 x0 y0 z0

          a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1
          n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1

          a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2
          n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 x2 y2 z2

          a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 k3 l3 m3
          n3 o3 p3 q3 r3 s3 t3 u3 v3 w3 x3 y3 z3))
   #f))

(banner "About a hundred keyword args")
(bench
 100
 (call/kw
  mr-bigglesworth 1 2 3
  (a0 9 b0 9 c0 9 d0 9 e0 9 f0 9 g0 9 h0 9 i0 9 j0 9 k0 9 l0 9 m0 9
      n0 9 o0 9 p0 9 q0 9 r0 9 s0 9 t0 9 u0 9 v0 9 w0 9 x0 9 y0 9 z0 9

      a1 9 b1 9 c1 9 d1 9 e1 9 f1 9 g1 9 h1 9 i1 9 j1 9 k1 9 l1 9 m1 9
      n1 9 o1 9 p1 9 q1 9 r1 9 s1 9 t1 9 u1 9 v1 9 w1 9 x1 9 y1 9 z1 9

      a2 9 b2 9 c2 9 d2 9 e2 9 f2 9 g2 9 h2 9 i2 9 j2 9 k2 9 l2 9 m2 9
      n2 9 o2 9 p2 9 q2 9 r2 9 s2 9 t2 9 u2 9 v2 9 w2 9 x2 9 y2 9 z2 9

      a3 9 b3 9 c3 9 d3 9 e3 9 f3 9 g3 9 h3 9 i3 9 j3 9 k3 9 l3 9 m3 9
      n3 9 o3 9 p3 9 q3 9 r3 9 s3 9 t3 9 u3 9 v3 9 w3 9 x3 9 y3 9 z3 9)))
