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
(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))
            
;; CHANGE (put your solutions here)
;; Problem 2
(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

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
        [(fun? e) (closure env e)]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(ifgreater? e)
         (let ([e1 (eval-under-env (ifgreater-e1 e) env)]
               [e2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? e1)
                    (int? e2))
               (if (> (int-num e1)
                      (int-num e2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "subexpressions of ifgreater are not int")))]
        [(mlet? e)
         (let* ([key (mlet-var e)]
               [val (eval-under-env (mlet-e e) env)]
               [extended-env (cons (cons key val) env)])
           (eval-under-env (mlet-body e) extended-env))]
        [(call? e)
         (let ([e1 (eval-under-env (call-funexp e) env)]
               [e2 (eval-under-env (call-actual e) env)])
           (if (closure? e1)
               (let* ([fn (closure-fun e1)]
                     [nameopt (fun-nameopt fn)]
                     [args (cons (fun-formal fn) e2)]
                     [body (fun-body fn)]
                     [extended-env (append env (if nameopt
                                               (list (cons nameopt e1) args)
                                               (list args)))])
                 (eval-under-env body extended-env))
               (error "first expression must be a closure")))]
        [(apair? e)
         (let ([e1 (eval-under-env (apair-e1 e) env)]
               [e2 (eval-under-env (apair-e2 e) env)])
           (apair e1 e2))]
        [(fst? e)
         (if (apair? (fst-e e))
             (apair-e1 (fst-e e))
             (error "expression must be apair"))]
        [(snd? e)
         (if (apair? (snd-e e))
             (apair-e2 (snd-e e))
             (error "expression must be apair"))]
        [(isaunit? e)
         (if (aunit? e) (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (aunit? (eval-under-env e1 null))
      (eval-under-env e2 null)
      (eval-under-env e3 null)))

(define (mlet* lstlst e2)
  (define (extend-env xs env)
    (if (null? xs)
        env
        (extend-env (cdr xs)
         (cons
          (cons (car (car xs))
                (eval-under-env (cdr (car xs)) env)) env))))
  (eval-under-env e2 (extend-env lstlst null)))

(define (ifeq e1 e2 e3 e4)
  (let ([v1 (eval-under-env e1 null)]
        [v2 (eval-under-env e2 null)])
    (if (and (int? v1)
             (int? v2))
        (eval-under-env (if (= (int-num v1) (int-num v2)) e3 e4) null)
        (error "subexpressions must be ints"))))

;; Problem 4

(define mupl-map
  (fun "#f" "g"
       (fun "map" "xs"
            (ifeq (int 1) (isaunit? (var "xs"))
                  (aunit)
                  (apair (call "g" (fst (var "xs")))
                         (call "map" (snd (var "xs"))))))))

(call mupl-map (fun #f "x" (add (var "x") (int 7))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

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