#lang planet dyoo/simply-scheme:2
(define (square x) (* x x))
(define (squares numbers)
  (if (empty? (bf numbers))
      (square (first numbers))
      (se (square (first numbers)) (squares (bf numbers)))))