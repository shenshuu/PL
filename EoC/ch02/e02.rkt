(define e0
  (Let 'x (Int 32)
       (Prim '+ (list
                 (Let 'x (Int 10) (Var 'x))
                 (Var 'x)))))

(define e1
  (Let 'x (Int 10)
       (Prim '+ (list
                 (Let 'x (Var 'x) (Var 'x))
                 (Var 'x)))))

(define e2
  (Let 'x (Let 'x (Let 'x (Int 10) (Var 'x))
               (Prim '+ (list (Var 'x) (Int 32))))
       (Prim '+ (list (Var 'x) (Var 'x)))))

(define e3 (Let 'x
                (Prim '+ (list (Int 1) (Int 2)))
                (Prim '+ (list (Var 'x) (Int 5)))))
                
(= (interp_Lvar (Program '() ((uniquify-exp '()) e0)))
   (interp_Lvar (Program '() e0)))
(= (interp_Lvar (Program '() ((uniquify-exp '()) e1)))
   (interp_Lvar (Program '() e1)))
(= (interp_Lvar (Program '() ((uniquify-exp '()) e2)))
   (interp_Lvar (Program '() e2)))
(= (interp_Lvar (Program '() ((uniquify-exp '()) e3)))
   (interp_Lvar (Program '() e3)))

((uniquify-exp '()) e0)
((uniquify-exp '()) e1)
((uniquify-exp '()) e2)
((uniquify-exp '()) e3)