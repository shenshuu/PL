#lang racket

(define (list-of-numbers? xs)
  (cond [(null? xs) #t]
        [(and (list? xs) (number? (car xs)))
         (list-of-numbers? (cdr xs))]
        [else #f]))

(list-of-numbers? (list 3 4 5)) ; true
(list-of-numbers? (list (list 3 4) 4 5)) ; false
(list-of-numbers? 'x) ; false