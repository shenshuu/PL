(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset)) rest)))))

; this works because it inductively builds up
; subsets starting from the base case for every
; element in the subset starting from the last element