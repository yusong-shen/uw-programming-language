#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
    (if (<= low high)
        (cons low (sequence (+ low stride) high stride))
        null))

(define (string-append-map xs suffix)
    (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(null? xs) (error "list-nth-mod: empty list")]
          [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
    (if (<= n 0)
        null
        (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
    (letrec (
        [f (lambda (x)
            (cons x (lambda ()
                (if (= 0 (remainder (+ x 1) 5))
                    (g (+ x 1))
                    (f (+ x 1)))
                )))]
        [g (lambda (x)
            (cons (- 0 x) (lambda ()
                (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
    (letrec (
        [f (lambda () (cons "dan.jpg" g))]
        [g (lambda () (cons "dog.jpg" f))])
    f))

(define (stream-add-zero s)
    (lambda ()
        (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
    (letrec (
        [f (lambda (n) (cons
            (cons (list-nth-mod xs n) (list-nth-mod ys n))
                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec (
      [f (lambda (len i)
            (if (= len i)
                #f
                (letrec ([cur (vector-ref vec i)])
                    (if (pair? cur)
                        (if (equal? (car cur) v)
                            cur
                            (f len (+ i 1)))
                        (f len (+ i 1))))))])
    (f (vector-length vec) 0)))

(define (cached-assoc xs n)
    (letrec (
        [memo (make-vector n #f)]
        [i 0]
        [f (lambda (v)
            (letrec ([cached-result (vector-assoc v memo)])
                (if cached-result
                    cached-result
                    (let ([result (assoc v xs)])
                        (begin
                            (vector-set! memo i result)
                            (set! i (remainder (+ i 1) n))
                            result)))))])
    f))

(define-syntax while-less
    (syntax-rules (do)
        [(while-less e1 do e2)
            (letrec (
                [n e1]
                [f (lambda (i)
                    (if (< e2 i)
                        (f i)
                        #t))])
            (f n))]))