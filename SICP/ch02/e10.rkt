(define (div-interval x y)
  (if (and (>= (upper-bound y) 0.0)
           (<= (lower-bound y) 0.0))
      (error 'division-by-zero "interval ~a cannot span zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))