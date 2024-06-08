(define (fringe xs)
  (cond ((null? xs) null)
        ((pair? xs) 
         (append (fringe (car xs))
                 (fringe (cdr xs))))
        (else (list xs))))

(fringe '((1 2) (3 4)))