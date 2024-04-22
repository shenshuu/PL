#define racket

(define (new-cons n)
  (* 2 n))

(define (car n)
  (if (= 0 (remainder n 3))
      (car (/ n 3))
      (/ (log n) (log 2))))

(define (cdr n)
  (if (= 0 (remainder n 2))
      (cdr (/ n 2))
      (/ (log n) (log 3))))