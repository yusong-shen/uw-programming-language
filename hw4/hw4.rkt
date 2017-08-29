
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Helper functions
; (define ones (lambda () (cons 1 ones)))
; (define a 2)

; p1
; (sequence 3 7 2) -> [3, 5, 7]
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; p2
(define (string-append-map xs suffix)
  (map (lambda (str)
         (string-append str suffix)) xs))


; p3
; wrong version
;(define (list-nth-mod xs n)
;  (cond [(null? xs) (error "list-nth-mod: empty list")]
;        [(< n 0) (error "list-nth-mod: negative number")]
;        [(= n 0) (car xs)]
;        [#t (list-nth-mod (cdr xs) (remainder (- n 1) (length xs)))]))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))


; p4
; stream is a thunk that will produce a pair (first_element, rest_of_the_thunk)
; thunk is a function with zero argument, it's used to delay calling, calling thunk will get the actual value
(define (stream-for-n-steps s n)
  (let ([sval (s)]) ; excute s only once
    (cond [(= n 0) null]
          [(= n 1) (list (car sval))]
          [#t (cons (car sval) (stream-for-n-steps (cdr sval) (- n 1)))])))

; p5
; 1, 2, 3, 4, -5, 6, 7 ... -10, 11, ... -15, 16
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cond [(= (remainder x 5) 0) (cons (- 0 x) (lambda () (f (+ x 1))))]
                      [#t (cons x (lambda () (f (+ x 1))))]))])
    (lambda () (f 1))))

; p6
(define dan-then-dog
  (letrec ([f (lambda (start)
                (cons start (if (string=? start "dan.jpg")
                    (lambda () (f "dog.jpg"))
                    (lambda () (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

; p7
; input : take a stream as argument
; output : another stream
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s)))
        (stream-add-zero (cdr (s))))))

; p8
; input : two list
; output : stream
; example : xs '(1 2 3) ys '(a b) -> (1 . a) (2 . b) (3 . a) ...
; recursive helper function f return a pair : (first_elemetn . rest_of_thunk)
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (lambda () (f (+ 1 n)))))])
    (lambda () (f 0))))


; p9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
         (cond [(>= n (vector-length vec)) #f]
               [(not (pair? (vector-ref vec n))) (f (+ 1 n))]
               [(equal? v (car (vector-ref vec n))) (vector-ref vec n)]
               [#t (f (+ 1 n))]))])
    (f 0)))

; p10
;(define (cached-assoc xs n)
;  (lambda (v) (assoc v xs))) 

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pos 0]
           [f (lambda (v xs)
              (let ([result (vector-assoc v cache)])
                   (cond [(pair? result) result] ; if v is in cache
                         [#t (let ([val (assoc v xs)]) ; try to get value first time
                               (if (not val)
                                   #f
                                   (begin (vector-set! cache pos val)
                                          (set! pos (remainder (+ 1 pos) n))
                                          val)))])))])
    (lambda (v) (f v xs))))
                               
    

; p11
;(define-syntax while-less
;  (syntax-rules (do)
;    [(while-less e1 do body)
;     (let ([e e1])
;       (letrec ([loop (lambda ()
;                        (if (>= body e)
;                            #t
;                            (begin body (loop))))])
;         (loop)))]))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do body)
     (let ([e e1])
       (letrec ([loop (lambda ()
                        (let ([v body])
                         (if (>= v e)
                             #t
                             (begin v (loop)))))])
         (loop)))]))

;while-less: Evaluates e2 the correct number of times [incorrect answer]
;vector-assoc: Lookup should succeed (Result of (vector-assoc (quote blah) (vector (cons (quote blah) 2) (cons (quote blah) 3))) was expected to equal '(blah . 2)) [incorrect answer]
;vector-assoc: Lookup skips non-pairs (Result of (vector-assoc 5 (vector (quote blah) something (lambda () (quote blah)) (cons -5 3) (cons 5 2))) was expected to equal '(5 . 2)) [incorrect answer]
;vector-assoc: Uses specified library functions (Uses vector-length) [incorrect answer]
;cached-assoc: Full caching implementation (creates a vector once) [incorrect answer]
;cached-assoc: Uses vector-set! to add a new pair to the cache (caddar: contract violation   expected: (cons/c (cons/c any/c (cons/c any/c pair?)) any/c)   given: '()) [error]
;cached-assoc: Checks cache for answer first before using assoc (submission uses assoc after finding element in cache) [incorrect answer]
;cached-assoc: Creates a new vector of correct size filled with #f (Doesn't work with n=2) [incorrect answer]
;
;Because the auto-grader gave a score above 80, here is the link to a message from a very cute dog: https://drive.google.com/file/d/0B5sUgbs6aDNpSWhSZzVtcktDaTA/view?pref=2&pli=1