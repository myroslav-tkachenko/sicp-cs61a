(load "./obj.scm")

;;; 7.1

(define-class (random-generator num)
  (instance-vars (counter 0))
  (method (number)
    (set! counter (+ counter 1))
    (random num))
  (method (count)
    (ask self 'counter)))


;;; 7.2

(define-class (coke-machine cokes price)
  (instance-vars (deposited 0))
  (method (deposit amount)
    (set! deposited (+ deposited amount)))
  (method (coke)
    (cond ((< deposited price)
          "NOT ENOUGH MONEY")
          ((> cokes 0)
            (set! deposited (- deposited price))
            (set! cokes (- cokes 1))
            deposited
          )))
  (method (fill amount)
    (set! cokes (+ cokes amount))))

(define my-machine (instantiate coke-machine 80 70))


;;; 7.3

(define ordered-deck '(AH 2H 3H QH KH AS 2S QC KC))

;(define (shuffle deck)
;  (if (null? deck)
;    '()
;  (let ((card (nth (random (length deck)) deck)))
;    (cons card (shuffle (remove card deck))) )))
(define (shuffle deck) deck)

(define-class (deck)
  (instance-vars (shuffled-deck '()))
  (initialize (set! shuffled-deck (shuffle ordered-deck)))
  (method (empty?)
    (= 0 (length shuffled-deck)))
  (method (get-card)
    (let ((card (car shuffled-deck)))
      (set! shuffled-deck (cdr shuffled-deck))
	  card))
  (method (deal)
	(if (ask self 'empty?)
	  '()
	  (ask self 'get-card))))


;;; 7.4

(define-class (person)
  (method (hello str)
    (list 'HELLO str)))

(define-class (miss-manners obj)
  (method (please m a)
    (ask obj m a)))
    
