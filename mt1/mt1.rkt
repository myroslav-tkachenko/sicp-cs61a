#lang planet dyoo/simply-scheme:2

(define (shorter? a b)
  (< (count a) (count b)))

(define (in-order? p s)
  (cond ((< (count s) 2))
        ((= (count s) 2) (p (first s) (first (bf s))))
        (else (and (p (first s) (first (bf s))) (in-order? p (bf s))))))

(define (order-checker p)
  (lambda (s) (in-order? p s)))