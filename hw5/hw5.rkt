;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
;; (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit)))
(define (racketlist->mupllist rkt-xs)
  (if (null? rkt-xs)
      (aunit)
      (apair (car rkt-xs) (racketlist->mupllist (cdr rkt-xs)))))
      

;; (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4))
(define (mupllist->racketlist mupl-xs)
  (if (aunit? mupl-xs)
      null
      (cons (apair-e1 mupl-xs) (mupllist->racketlist (apair-e2 mupl-xs)))))


;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
;; env is a list of pair (var_name_string, var_value)
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e)
         (let ([v (int-num e)])
           (if (number? v)
               (int v)
               (int (eval-under-env e env))))]
        [(closure? e) e] ; closure is a value
        [(fun? e) ; TODO : function should be evaluated to closure with initial empty environment ?
         (closure env e)]
        [(ifgreater? e) ; if e1 > e2 then e3 else e4
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2)) ; if both values of e1 and e2 are integer
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater e1 and e2 are non-number")))]
        [(mlet? e) 
         (letrec ([v (eval-under-env (mlet-e e) env)]
               [v-name (mlet-var e)]
               [new-env (cons (cons v-name v) env)]) ; add (var, v) to env
           (eval-under-env (mlet-body e) new-env))] ; evaluate body with local binding (var = e)
        [(call? e) ; (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1)))
         (let ([func (eval-under-env (call-funexp e) env)] ; use current environment to evaluate e1 to a closure
               [param-val (eval-under-env (call-actual e) env)]) ; use current environment to evaluate call-actual to value
           (if (closure? func)
               (letrec ([body (closure-fun func)]
                     [f-name (fun-nameopt body)]
                     [param-name (fun-formal body)]
                     [f-body (fun-body body)] ; evaluate the closure's function body in the extended environment
                     [anoymous-env (cons (cons param-name param-val) (closure-env func))] ; add the argument binding to environment
                     [extended-env (if (not f-name)
                                       anoymous-env
                                       (cons (cons f-name func) anoymous-env))]) ; TODO : if it's not anoymous function, map the function's name to its closure (if f-name has not existed in environment yet), used for recursive call
                 (eval-under-env f-body extended-env)) ; TODO : evaluate the function body in the extended environment
               (error "MUPL call's first argument should be closure")))]
        [(aunit? e) e]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (if (apair? (fst-e e))
             (eval-under-env (apair-e1 (fst-e e)) env)
             (error "MUPL : get a non pair expression"))]
        [(snd? e)
         (if (apair? (snd-e e))
             (eval-under-env (apair-e2 (snd-e e)) env)
             (error "MUPL : get a non pair expression"))]
        [(isaunit? e)
         (if (aunit? (isaunit-e e)) (int 1) (int 0))] ; Note : (aunit) is a MUPL expression while aunit is not
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
; input : take 3 MUPL expression e1, e2 and e3
; output : return a MUPL expression that if e1 is aunit, then e2 else e3
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

; input : take a list of Racket pairs and a final MUPL expression e2
; output : perform local binding like let* in Racket and return the value by evaluating e2 with new environment
(define (mlet* lstlst e2)
  (if (null? lstlst) ; base case : if there is no local binding, just return e2
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) ; perform local binding for the first pair
            (mlet* (cdr lstlst) e2))))

; input : take 4 MUPL expression e1, e2, e3 and e4
; if e1 and e2 are evaluated as equal integer, then e3 else e4
; Note : e1 and e2 should be evaluated exactly once
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y") e4
                         (ifgreater (var "_y") (var "_x") e4 e3)))))
                         

;; Problem 4
; mupl-map is a variable binded to MUPL function, it should be curried
; MUPL function's input : a MUPL function with one argument like (lambda (x) (+ 7 x))
; ouput : a MUPL function that takes a MUPL list and applies the function to every element of the list
;     returning a new MUPL list
;   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
;                 (apair (int 8) (aunit)) "mupl-map test")
 (define mupl-map
   (fun #f "lambda" ; return a function that take a lambda function
        (fun "map" "mupl-list"  ; the upper level function return another function that take MUPL list as argument
             (ifaunit (var "mupl-list") (aunit) ; the returned function applied lambda to each element of MUPL list - base case for empty list
                      (apair (call (var "lambda") (fst (var "mupl-list"))) ; process the first element of "mupl-list"
                             (call (var "map") (snd (var "mupl-list")))))))) ; recursively call mupl-map to process the rest of the "mupl-list"
  
  
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))