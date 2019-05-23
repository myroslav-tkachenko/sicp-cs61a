#lang planet dyoo/simply-scheme:2

(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square x) (* x x))

;;; 2.30

;(define (square-tree t)
;  (map (lambda (sub-tree)
;         (if (list? sub-tree)
;             (square-tree sub-tree)
;             (* sub-tree sub-tree))) t))


;;; 2.31

(define (tree-map p t)
  (define (traverse tree)
    (map (lambda (sub-tree)
           (if (list? sub-tree)
               (traverse sub-tree)
               (p sub-tree))) tree))
  ((lambda (in-tree) (traverse in-tree)) t))


;;; 2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (x)
                        (cons (car s) x))
                      rest)))))
