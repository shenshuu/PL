(define (accumulate op acc sequence)
  (if (null? sequence)
      acc
      (op (car sequence)
          (accumulate op acc (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5))

; (+ 5 (* 0 2)) => 5
; (+ 0 (* 5 2)) => 10
; (+ 3 (* 10 2)) => 23
; (+ 1 (* 23 2)) => 47