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

(define (bubble-sort! vec)
	(define (swap i)
		(let ((a (vector-ref vec i))
			 (b (if (= (+ i 1) (vector-length vec))
				 #f
                 (vector-ref vec (+ i 1)))))
			 (if (and b (> a b))
				 (begin
                 	(vector-set! vec i b)
                    (vector-set! vec (+ i 1) a))
				 vec)))
                  
	(define (sort to)
		(define (move-items i)
			(if (> i to)
				vec
				(begin
				   (swap i)
                   (move-items (+ i 1)))))
		(if (<= to 0) 
			vec
			(begin
				(move-items 0)
				(sort (- to 1)))))
	(sort (- (vector-length vec) 1))
    vec)
