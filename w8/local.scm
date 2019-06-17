(define (make-account balance)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount))
				balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount))
		balance)
	(define (dispatch m)
		(cond ((eq? m 'withdraw) withdraw)
			((eq? m 'deposit) deposit)
			(else (error "Unknown request: MAKE-ACCOUNT"
				m))))
	dispatch)


;;; 3.3

(define (make-account balance password)
	(let ((pass-count 0))
		(define (withdraw amount)
			(set! pass-count 0)
			(if (>= balance amount)
				(begin (set! balance (- balance amount))
					balance)
				"Insufficient funds"))
		(define (deposit amount)
			(set! pass-count 0)
			(set! balance (+ balance amount))
			balance)
		(define (wrong-pass amount)
			(set! pass-count (+ pass-count 1))
			(if (> pass-count 7)
				(begin (display "call-the-cops")
					(newline)
					"Incorrect password")
				"Incorrect password"))
		(define (dispatch p m)
			(cond 
				((not (eq? p password)) wrong-pass)
				((eq? m 'withdraw) withdraw)
				((eq? m 'deposit) deposit)
				(else (error "Unknown request: MAKE-ACCOUNT" m))))
		dispatch))


;;; 3.7

(define (make-joint acc pass new-pass)
	(define (wrong-pass amount) "Incorrect password")
	(define (dispatch p m)
		(if	(not (eq? p new-pass) wrong-pass
			(acc pass m))))
	dispatch)


;;; 3.8

(define (memoize-call)
	(let ((n 1))
		(lambda (a)
			(set! n (* n a))
			n)))

(define f (memoize-call))

