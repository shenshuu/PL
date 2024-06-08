(define (deep-reverse xs)
  (if (pair? xs)
      (reverse (map deep-reverse xs))
      xs))

(deep-reverse '((1 2) (3 4)))