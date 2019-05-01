#lang planet dyoo/simply-scheme:2

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial m)
  (define (inc n) (+ n 1))
  (define (identity x) x)
  (product identity 1 inc m))

(define (pi-approx m)
  (define (inc-2 n) (+ n 2))
  (define (identity-odd x) (/ x (+ x 1)))
  (define (identity-even x) (/ (+ x 1) x))
  (* (product identity-odd 2 inc-2 m) (product identity-even 3 inc-2 m)))

(define (inc n) (+ n 1))
(define (identity x) x)
(define (mult a b) (* a b))
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (is-even? x) (even? x))
(define (filtered-accumulate predicate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (predicate (term a)) (term a) null-value)
         (filtered-accumulate predicate combiner null-value term (next a) next b))))


