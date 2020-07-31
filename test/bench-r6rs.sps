(import (chezscheme) (srfi :177))

(define fib
  (lambda (n)
    (if (<= n 2) 1 (+ (fib (- n 2))
                      (fib (- n 1))))))

(define key-fib
  (lambda/kw ((n))
    (if (<= n 2) 1 (+ (call/kw key-fib (n (- n 2)))
                      (call/kw key-fib (n (- n 1)))))))

(time (fib 40))
(time (call/kw key-fib (n 40)))


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

(time
 (let loop ((n 100000))
   (when (> n 0)
     (call/kw
      mr-bigglesworth 1 2 3
      (a0 n b0 n c0 n d0 n e0 n f0 n g0 n h0 n i0 n j0 n k0 n l0 n m0 9
          n0 n o0 n p0 n q0 n r0 n s0 n t0 n u0 n v0 n w0 n x0 n y0 n z0 9

          a1 n b1 n c1 n d1 n e1 n f1 n g1 n h1 n i1 n j1 n k1 n l1 n m1 9
          n1 n o1 n p1 n q1 n r1 n s1 n t1 n u1 n v1 n w1 n x1 n y1 n z1 9

          a2 n b2 n c2 n d2 n e2 n f2 n g2 n h2 n i2 n j2 n k2 n l2 n m2 9
          n2 n o2 n p2 n q2 n r2 n s2 n t2 n u2 n v2 n w2 n x2 n y2 n z2 9

          a3 n b3 n c3 n d3 n e3 n f3 n g3 n h3 n i3 n j3 n k3 n l3 n m3 9
          n3 n o3 n p3 n q3 n r3 n s3 n t3 n u3 n v3 n w3 n x3 n y3 n z3 9))
     (loop (- n 1)))))
