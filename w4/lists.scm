#lang planet dyoo/simply-scheme:2

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(define (length items)
  (if (null? items)
      0
      (+ (length (cdr items)) 1)))

(define odds (list 1 3 5 7))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse seq)
  (if (null? seq)
      '()
      (append (reverse (cdr seq)) (list (car seq)))))

;;; 2.17
(define (last-pair list)
  (if (= (length list) 0) '()
      (list-ref list (- (length list) 1))))

;;; 2.20
;(define (same-parity . l)
;  (define (make-list out-list in-list parity)
;    (if (null? in-list)
;        out-list
;        (if (= (remainder (car in-list) 2) parity)
;            (make-list (append out-list (list (car in-list))) (cdr in-list) parity)
;            (make-list out-list (cdr in-list) parity))))
;  (make-list '() l (remainder (car l) 2)))

(define (same-parity . numbers)
  (define (make-list items parity)
    (if (null? items)
        '()
        (if (= (remainder (car items) 2) parity)
            (cons (car items) (make-list (cdr items) parity))
            (make-list (cdr items) parity))))
  (make-list numbers (remainder (car numbers) 2)))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

;;; 2.23
(define (for-each proc items)
  (define res (if (null? items) #t (proc (car items))))
  (if (null? items)
      #t
      (for-each proc (cdr items))))