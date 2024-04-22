#lang racket

(define (new-cons x y)
  (lambda (m) (m x y)))

(define (new-car z)
  (z (lambda (p q) p)))

(define (new-cdr z)
  (z (lambda (p q) (new-cons q '()))))

(new-car (new-cons 3 4))
(new-car (new-cdr (new-cons 3 4)))