(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define (total-length mobile)
  (if (not (pair? mobile))
      0
      (+ (branch-length (left-branch mobile))
         (branch-length (right-branch mobile))
         (total-length (branch-structure (left-branch mobile)))
         (total-length (branch-structure (right-branch mobile))))))

(define (total-torque mobile)
  (* (total-weight mobile) (total-length mobile)))

(define (balanced? mobile)
  (let ((left-torque (total-torque (left-branch mobile)))
        (right-torque (total-torque (right-branch mobile))))
    (and (= left-torque right-torque)
         (balanced? (left-branch mobile))
         (balanced? (right-branch mobile)))))

(define m0 (make-mobile (make-branch
                         3
                         (make-mobile
                          (make-branch 1 2)
                          (make-branch 3 4)))
                        (make-branch 3 5)))

(total-weight m0)
(total-length m0)
(balanced? m0)

; we would only need to change the definitions
; of the constructors for the second part of 
; the pair with cdr instead of cadr