(load "./sequence.scm") 
;;; 3.17

(define (in-list? e l)
	(not (null? (filter (lambda (el) (eq? el e)) l))))
	
(define l (list 1 2 3))


(define (count-pairs x)
	(define (cp x l)
		(if (or (not (pair? x)) (in-list? x l))
			0
			(+ (cp (car x) (append l x))
				(cp (cdr x) (append l x))
				1)))
	(cp x '()))
				


(define (count-pairs x)
	(let ((counted '()))
		(define (cp x)
			(cond ((memq x counted) 0)
				((not (pair? x)) 0)
				(else (begin
					(set! counted (cons x counted))
					(+ (cp (car x)) (cp (cdr x)) 1)))))
		(cp x)))
