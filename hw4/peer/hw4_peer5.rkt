
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
	(define (sequence2 next accum)
		(if (> next high) accum (sequence2 (+ next stride) (cons next accum)))
	)
	(if (< high low) (list) (reverse (sequence2 low (list))))
)

(define (string-append-map xs suffix)
	(map (lambda (x) (string-append x suffix)) xs)
)

(define (list-nth-mod xs n)
	(cond 	[(< n 0)	(error "list-nth-mod: negative number")]
			[(null? xs)	(error "list-nth-mod: empty list")]
			[#t			(let ([i (remainder n (length xs))])
							 (car (list-tail xs i))
						)
			]
	)
)

(define (stream-for-n-steps stream number)
	(define (extract-prepend stream number accum)
		(if (= number 0) accum (let ([temp (stream)]) (extract-prepend (cdr temp) (- number 1) (cons (car temp) accum))))
	)
	(reverse (extract-prepend stream number (list)))
)

(define (funny-number-stream)
	(define (generate-stream x)
		(let ([next (+ (abs x) 1)])
			 (cons x (lambda () (generate-stream (if (= 0 (remainder next 5)) (- 0 next) next))))
		)
	)
	(generate-stream 1)
)

(define (dan-then-dog)
	(define (dan-then-dog2 dan?)
		(cons (if dan? "dan.jpg" "dog.jpg") (lambda () (dan-then-dog2 (not dan?))))
	)
	(dan-then-dog2 #t)
)

(define (stream-add-zero stream)
	(define (add-zero stream)
		(let ([eval (stream)])
			 (cons (cons 0 (car eval)) (lambda () (add-zero (cdr eval))))
		)
	)
	(lambda () (add-zero stream))
)

(define (cycle-lists xs ys)
	(define (cycle x-index y-index)
		(cons (cons (list-nth-mod xs x-index) (list-nth-mod ys y-index)) (lambda () (cycle (+ x-index 1) (+ y-index 1))))
	)
	(lambda () (cycle 0 0))
)

(define (vector-assoc value vector)
	(let ([len (vector-length vector)])
		(define (check-next current)
			(letrec ([item	(vector-ref vector current)]
					 [next	(+ current 1)]
					 [key	(if (pair? item) (car item) #f)]
					)
				(cond [(equal? key value) item]
					  [#t (if (>= next len) #f (check-next next))]
				)
			)
		)
		(if (< len 1) #f (check-next 0))
	)
)

(define (cached-assoc xs n)
	(let
		(
			[cache	(make-vector n #f)]
			[pos	0]
		)
		(define (retrieve key)
			(let ([exists (vector-assoc key cache)])
				(if exists exists
					(let ([entry (assoc key xs)])
						(if entry (begin (vector-set! cache pos entry) (set! pos (remainder (+ pos 1) n)) entry) #f)
					)
				)
			)
		)
		(lambda (key) (retrieve key))
	)
)

(define-syntax while-less
	(syntax-rules (do)
		[(while-less el do e2)
		 (letrec (
		 			[threshold el]
		 			[proceed   (lambda () (if (< e2 threshold) (proceed) #t))]
		 	  	 )
		 	(proceed)
		 )
		]
	)
)