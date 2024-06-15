(define (accumulate op acc sequence)
  (if (null? sequence)
      acc
      (op (car sequence)
          (accumulate op acc (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (_ y) (+ y 1)) 0 sequence))

(define list-of-squares (my-map (lambda (x) (* x x)) (list 1 2 3)))
(define appended-list (my-append (list 1 2 3) (list 4 5 6)))
(my-length appended-list)