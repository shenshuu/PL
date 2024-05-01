(define (remove_complex_operands env)
  (letrec ([rco_prim (lambda (e1 e2 op)
                       (define-values (x1 mapping1) (rco_atm e1))
                       (define-values (x2 mapping2) (rco_atm e2))
                       (match (list (or (Int? e1) (Var? e1))
                                    (or (Int? e2) (Var? e2)))
                         ['(#t #t) (Prim op (list e1 e2))]
                         ['(#t #f) (Let x2 (dict-ref mapping2 x2)
                                        (Prim op (list e1 (Var x2))))]
                         ['(#f #t) (Let x1 (dict-ref mapping1 x1)
                                        (Prim op (list (Var x1) e2)))]
                         ['(#t #t) (Let x1 (dict-ref mapping1 x1)
                                        (Let x2 (dict-ref mapping2 x2)
                                             (Prim op (list (Var x1) (Var x2)))))]))]
           [rco_atm (lambda (e)
                      (define tmp-var (string->symbol (format "tmp.~a" (gensym))))
                      (values tmp-var (list (cons tmp-var e))))]
           [rco_exp (lambda (e)
                      (match e
                        [(Int n) (Int n)]
                        [(Var x) (Var x)]
                        [(Prim '- (list _))
                         (define-values (x mapping) (rco_atm e))
                         (Let x (dict-ref mapping x) (Var x))]
                        [(Prim '+ (list e1 e2))
                         (rco_prim e1 e2 '+)]
                        [(Prim '- (list e1 e2))
                         (rco_prim e1 e2 '-)]
                        [(Let x e body)
                         (Let x (rco_exp e) (rco_exp body))]))])
    (lambda (e)
      (rco_exp e))))