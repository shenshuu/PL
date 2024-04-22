#lang racket

(require racket/fixnum)

(struct Program (env exp) #:transparent)
(struct Prim (op args) #:transparent)
(struct Int (value) #:transparent)
(struct Var (symbol) #:transparent)
(struct Let (x e body) #:transparent)

(define interp-Lint-class
  (class object%
    (super-new)
    
    (define/public ((interp_exp env) e)
      (match e
        [(Int n) n]
        [(Prim 'read '())
         (define r (read))
         (cond [(fixnum? r) r]
               [else (error 'interp_exp "read expected an integer" r)])]
        [(Prim '- (list e)) (fx- 0 ((interp_exp env) e))]
        [(Prim '+ (list e1 e2))
         (fx+ ((interp_exp env) e1) ((interp_exp env) e2))]
        [(Prim '- (list e1 e2))
         (fx- ((interp_exp env) e1) ((interp_exp env) e2))]))

    (define/public (interp_program p)
      (match p
        [(Program '() e) ((interp_exp '()) e)]))
    ))

(define interp-Lvar-class
  (class interp-Lint-class
    (super-new)
    
    (define/override ((interp_exp env) e)
      (match e
        [(Var x) (dict-ref env x)]
        [(Let x e body)
         (define new-env (dict-set env x ((interp_exp env) e)))
         ((interp_exp new-env) body)]
        [else
         ((super interp_exp env) e)]))))

(define (interp_Lvar p)
  (send (new interp-Lvar-class) interp_program p))

(define p0
  (Program '()
  (Let 'y (Prim '+ (list (Int 4) (Int 6))) (Prim '- (list (Var 'y))))))

(interp_Lvar p0)