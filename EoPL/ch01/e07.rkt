(define (nth-elt xs n)
  (define (helper ys k)
    (cond [(null? ys)
           (error 'invalid-index "~a does not an index ~a" xs n)]
          [(and (list? ys) (> k 0))
           (helper (cdr ys) (- k 1))]
          [(and (list? ys) (= k 0))
           (car ys)]
          [else
           (error 'invalid-argument "~a must be of type list" xs)]))
  (helper xs n))