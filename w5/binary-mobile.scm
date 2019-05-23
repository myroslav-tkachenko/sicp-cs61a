#lang planet dyoo/simply-scheme:2

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (mobile? structure)
  (list? structure))

(define (branch-weight branch)
  (if (and (not (mobile? (branch-structure branch)))
           (= (branch-length branch) 1))
      (branch-structure branch)
      (total-weight (branch-structure branch))))
   
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balanced? mobile)
  (= (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define l-branch (make-branch 1 3))
(define r-branch (make-branch 1 4))
(define m1 (make-mobile l-branch r-branch))
(define m2 (make-mobile l-branch r-branch))
(define m (make-mobile
           (make-branch 1 m1)
           (make-branch 1 m2)))
