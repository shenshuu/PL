(define (accumulate op acc sequence)
  (if (null? sequence)
      acc
      (op (car sequence)
          (accumulate op acc (cdr sequence)))))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (subtree)
                         (if (pair? subtree)
                             (count-leaves subtree)
                             1))
                       tree)))

(count-leaves (list (list 1 2 3) 3 (list 4)))