#lang planet dyoo/simply-scheme:2

(define (ends-e? w) (equal? (last w) 'e))
(define (ends-e s)
  (define (check-sent sent out)
    (if (equal? 1 (count sent))
        (if (ends-e? (first sent))
            (se out (first sent))
            (se out))
        (check-sent (bf sent) (if (ends-e? (first sent))
            (se out (first sent))
            (se out)))))
  (if (equal? 0 (count s))
      (se '())
      (check-sent s '())))
  