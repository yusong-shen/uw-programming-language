
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
 (if (<= n 0)
     null
     (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (modulo x 5) 0)
                              (cons (- 0 x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if (odd? x)
                              (cons "dan.jpg" (lambda () (f (+ x 1))))
                              (cons "dog.jpg" (lambda () (f (+ x 1))))))])
  (lambda () (f 1))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x s) (cons (cons 0 (car (s))) (lambda () (f (+ x 1) (cdr (s))))))])
    (lambda () (f 1 s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda() (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n) (cond [(>= n (vector-length vec)) #f]
                             [(pair? (vector-ref vec n)) (if (equal? v (car (vector-ref vec n)))
                                                             (vector-ref vec n)
                                                             (f (+ n 1)))]
                             [#t (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [next-slot 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (vector-set! memo next-slot new-ans)
                              (set! next-slot (remainder (+ next-slot 1) n))
                              new-ans)
                            #f)))))])
  f))

;Challenge problem
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([n e1])
       (letrec ([loop (lambda ()
                       (let ([m e2])
                         (if (or (not (number? m)) (>= m n))
                             #t
                             (loop))))])
          (loop)))]))
(define ones (lambda () (cons 1 ones)))
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))