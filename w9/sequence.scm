;#lang planet dyoo/simply-scheme:2

(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (odd? x) (not (= (remainder x 2) 0)))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))


;;; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define l (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))


;;; 2.37

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))


;;; 2.54

(define (equal? a b)
  (define (compare fa fb r)
    (if (empty? fa)
        r
        (compare (cdr fa) (cdr fb) (and r (eq? (car fa) (car fb))))))
  
  (let ((flat-a (enumerate-tree a))
        (flat-b (enumerate-tree b))
        (count-flat-a (count (enumerate-tree a)))
        (count-flat-b (count (enumerate-tree b))))
    (if (not (= count-flat-a count-flat-b))
        #f
        (compare flat-a flat-b #t))))
