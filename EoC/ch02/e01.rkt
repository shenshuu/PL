(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x) (Var (dict-ref env x (format "~a.~a" x (gensym))))]
      [(Int n) (Int n)]
      [(Let x e body)
       (define new-x (format "~a.~a" x (gensym)))
       (Let new-x
            ((uniquify-exp env) e)
            ((uniquify-exp (cons (cons x new-x) env)) body))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))
