#lang planet dyoo/simply-scheme:2

(define (sum-of-factors sent)
  (define (sum-iter sent a)
    (if (empty? sent)
        a
        (sum-iter (bf sent) (+ a (first sent)))))
  (sum-iter sent 0))

(define (get-factors n)
  (define (factors-iter f i)
    (if (= i 0)
        f
        (if (and (= (remainder n i) 0) (< i n))
            (factors-iter (se f i) (- i 1))
            (factors-iter f (- i 1)))))
  (factors-iter (se '()) n))

(define (next-perf n)
  (if (= (sum-of-factors (get-factors n)) n)
      n
      (next-perf (+ n 1))))