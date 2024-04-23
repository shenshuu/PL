; <top-level>         := <s-list>
; <s-list>            := {<symbol-expression>}*
; <symbol-expression> := <symbol> | <s-list>

(define notate-depth-in-s-list
  (lambda (slist d)
    (map
     (lambda (se)
       (if (symbol? se)
           (list se d)
           (notate-depth-in-s-list se (+ d 1))))
     slist)))