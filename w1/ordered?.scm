#lang planet dyoo/simply-scheme:2

(define (ordered? sent)
  (cond ((< (count sent) 2) #t)
        ((equal? (count sent) 2) (< (first sent) (first (bf sent))))
        (else (and (< (first sent) (first (bf sent))) (ordered? (bf sent))))))