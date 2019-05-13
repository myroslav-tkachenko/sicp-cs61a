#lang planet dyoo/simply-scheme:2

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter b (/ n 2) (* a (square a))))
          (else (iter b (- n 1) (* b a)))))
  (iter b n 1))