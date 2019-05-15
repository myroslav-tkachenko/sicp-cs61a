#lang planet dyoo/simply-scheme:2

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C) (word 'JK) (word 'JK)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 54) )

(define (is-ace? card)
  (or (equal? (first card) 'A) (equal? (first card) 'a)))

(define (card-value card)
  (cond ((is-ace? card) 11)
        ((or (equal? (first card) 'J) (equal? (first card) 'j)) 10)
        ((or (equal? (first card) 'Q) (equal? (first card) 'q)) 10)
        ((or (equal? (first card) 'K) (equal? (first card) 'k)) 10)
        ((equal? (first card) '1) 10)
        (else (first card))))

(define (exclude-aces hand)
    (if (empty? (bf hand))
        (if (is-ace? (first hand))
            (se '())
            (se (first hand)))
        (if (is-ace? (first hand))
            (se '() (exclude-aces (bf hand)))
            (se (first hand) (exclude-aces (bf hand))))))

(define (extract-aces hand)
    (if (empty? (bf hand))
        (if (is-ace? (first hand))
            (se (first hand))
            (se '()))
        (if (is-ace? (first hand))
            (se (first hand) (extract-aces (bf hand)))
            (se '() (extract-aces (bf hand))))))

(define (extract-jokers hand)
    (if (empty? (bf hand))
        (if (or (equal? (first hand) 'JK) (equal? (first hand) 'jk))
            (se (first hand))
            (se '()))
        (if (or (equal? (first hand) 'JK) (equal? (first hand) 'jk))
            (se (first hand) (extract-jokers (bf hand)))
            (se '() (extract-jokers (bf hand))))))

(define (has-jokers hand)
  (not (empty? (extract-jokers hand))))

(define (best-total hand)
  (define (sum-iter hand s)
    (if (empty? hand)
        s
        (sum-iter (bf hand) (+ (card-value (first hand)) s))))
  (define (add-aces-iter hand s)
    (if (empty? hand)
        s
        (add-aces-iter (bf hand)
                       (cond ((and (empty? (bf hand)) (= (+ s 11) 21)) (+ s 11))
                             ((and (empty? (bf hand)) (< (+ s 11) 21)) (+ s 11))
                             ((< (+ s 11) 21) (+ s 11))
                             (else (+ s 1))))))

  (cond ((and (= (length (extract-jokers hand)) 2) (= (length hand) 2)) 21)
        ((= (length (extract-jokers hand)) 0) (add-aces-iter (extract-aces hand) (sum-iter (exclude-aces hand) 0)))
        (else 10)))

(define (stop-at-17 hand dealer-card)
  (< (best-total hand) 17))

(define (dealer-sensitive hand dealer-card)
  (or (and (> (card-value dealer-card) 6) (< (best-total hand) 17))
      (and (< (card-value dealer-card) 7) (< (best-total hand) 12))))

;(define (valentine hand dealer-card)
;  (define (is-heart? card)
;    (or (equal? (last card) 'h)
;        (equal? (last card) 'H)))
;  (define (has-heart-iter? hand r)
;    (if (empty? hand)
;        r
;        (has-heart-iter? (bf hand) (or r (is-heart? (first hand))))))
;  (if (has-heart-iter? hand #f)
;      (< (best-total hand) 19)
;      (< (best-total hand) 17)))

(define (stop-at n)
  (lambda (hand dealer-card) (< (best-total hand) n)))

(define (suit-strategy suit strategy-n strategy-i)
  (define (is-suit? card)
    (equal? (last card) suit))
  (define (has-suit-iter? hand r)
    (if (empty? hand)
        r
        (has-suit-iter? (bf hand) (or r (is-suit? (first hand))))))
  (lambda (hand dealer-card)
    (if (has-suit-iter? hand #f)
      (strategy-i hand dealer-card)
      (strategy-n hand dealer-card))))

(define (majority s1 s2 s3)
  (lambda (hand dealer-card)
    (or (and (s1 hand dealer-card) (s2 hand dealer-card))
        (and (s2 hand dealer-card) (s3 hand dealer-card))
        (and (s1 hand dealer-card) (s3 hand dealer-card)))))

(define (valentine hand dealer-card)
  ((suit-strategy 'H (stop-at 17) (stop-at 19)) hand dealer-card))

(define (reckless s)
  (lambda (hand dealer-card)
    (s (butlast hand) dealer-card)))

(define (play-n strategy n)
  (define (play count res)
    (if (< count 1)
        res
        (play (- count 1) (+ res (twenty-one strategy)))))
  (play n 0))
      

;                                      32