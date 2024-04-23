(define duple
  (lambda (n x)
    (if (= n 0)
        '()
        (cons x (duple (- n 1) x)))))

(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (cadar lst) (caar lst))
              (invert (cdr lst))))))

(define filter-in
  (lambda (pred lst)
    (cond [(null? lst) '()]
          [(pred (car lst)) (cons (car lst) (filter-in pred (cdr lst)))]
          [else (filter-in pred (cdr lst))])))

(define every
  (lambda (pred lst)
    (if (null? lst)
        #t
        (and (pred (car lst)) (every pred (cdr lst))))))

(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (or (pred (car lst)) (exists? pred (cdr lst))))))

(define vector-index
  (lambda (pred v)
    (cond [(= (vector-length v) 0) #f]
          [(pred (vector-ref v 0)) (vector-ref v 0)]
          [else (vector-index pred (vector-drop v 1))])))

(define list-set
  (lambda (lst n x)
    (cond [(null? lst) '()]
          [(= n 0) (cons x (list-set (cdr lst) (- n 1) x))]
          [else (cons (car lst) (list-set (cdr lst) (- n 1) x))])))

(define flatmap
  (lambda (proc lst)
    (apply append (map proc lst))))

(define product
  (lambda (los1 los2)
    (flatmap (lambda (x1)
           (map (lambda (x2)
                  (list x1 x2))
                los2))
         los1)))

(define down
  (lambda (lst)
    (map list lst)))