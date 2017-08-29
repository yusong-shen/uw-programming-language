
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; Problem 1

(define (sequence low high stride)
  (if ( > low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (i) (string-append i suffix)) xs))

; Problem 3
(define (list-nth-mod xs n)
  (if (< n 0) (error "list-nth-mod: negative number")
      (if (null? xs) (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons
                           (if (= (remainder x 5) 0)
                               (* -1 x) x)
                           (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons
                           (if (= (remainder x 2) 0) "dog.jpg" "dan.jpg")
                           (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; Problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda(st) (cons (cons 0 (car (st))) (lambda () (f (cdr (st))))))])
    (lambda () (f s))))

; Problem 8
(define (cycle-lists xs ys)
  (letrec ([ f (lambda (n) (cons
                            (cons (list-nth-mod xs n) (list-nth-mod ys n))
                            (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

; Problem 9
(define (vector-assoc v vec)
  (letrec ([va (lambda (n)
                 (if (= n (vector-length vec)) #f
                 (if (and (pair? (vector-ref vec n)) (equal? v (car (vector-ref vec n))))
                     (vector-ref vec n)
                     (va (+ n 1)))))])
    (if (vector? vec) (va 0) #f)))

; Problem 10
(define (cached-assoc xs n)
  (letrec([v (make-vector n)] ; cache vector
          [p 0] ; position in cache to be updated
          [f (lambda (xs)
               (lambda (x)
               (let ([ans (vector-assoc x v)])
                 (if ans ans
                     (let ([new-ans (assoc x xs)])
                       (if new-ans
                           (begin
                             (vector-set! v p new-ans)
                             (set! p (if (= (+ p 1) n) 0 (+ p 1)))
                             new-ans)
                           new-ans))))))])
    (f xs)))
    