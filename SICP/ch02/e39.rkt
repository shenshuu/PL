(define (reverse-via-foldr sequence)
  (foldr (lambda (x y) (append y (list x))) null sequence))

(define (reverse-via-foldl sequence)
  (foldl cons null sequence))