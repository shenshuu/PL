(define subst
  (lambda (new old slist)
    (map
     (lambda (se)
       (if (null? se)
           '()
            (if (symbol? se)
                (if (eqv? se old) new se)
                (subst new old se))
            ))
     slist)))