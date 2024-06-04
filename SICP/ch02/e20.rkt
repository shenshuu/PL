(define (same-parity x . y)
  (cons x (filter (lambda (z)
                    (= (remainder x 2)
                       (remainder z 2)))
                  y)))

(same-parity 1 2 3 4 5 6 7)