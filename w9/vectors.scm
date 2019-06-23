(define (vector-append a b)
	(let ((new-vector (make-vector (+ (vector-length a) (vector-length b)))))
		(define (loop i offset v)
			(if (= i (vector-length v))
				new-vector
				(begin
					(vector-set! new-vector (+ i offset) (vector-ref v i))
					(loop (+ i 1) offset v))))
		(loop 0 0 a)
		(loop 0 (vector-length a) b)
		new-vector))

(define (vector-filter pred vec)
	(let ((new-vector (make-vector 0)))
		(define (loop i)
			(if (= i (vector-length vec))
				new-vector
				(if (pred (vector-ref vec i))
					(begin
						(set! new-vector (vector-append new-vector (make-vector 1 (vector-ref vec i))))
						(loop (+ i 1)))
					(loop (+ i 1)))))
		(loop 0)
		new-vector))
