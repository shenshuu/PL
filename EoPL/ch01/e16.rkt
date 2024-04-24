(define up
  (lambda (lst)
    (cond ((null? lst) '())
          ((list? (car lst)) (append (car lst)
                                     (up (cdr lst))))
          (else (cons (car lst) (up (cdr lst)))))))

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (if (symbol? (car slist))
            (cons (cond ((eqv? s1 (car slist)) s2)
                        ((eqv? s2 (car slist)) s1)
                        (else (car slist)))
                  (swapper s1 s2 (cdr slist)))
            (cons (swapper s1 s2 (car slist))
                  (swapper s1 s2 (cdr slist)))))))

(define count-occurrences
  (lambda (s slist)
    (cond ((null? slist) 0)
          ((list? (car slist))
           (+ (count-occurrences s (car slist))
              (count-occurrences s (cdr slist))))
          (else (+ (if (eqv? s (car slist)) 1 0)
                   (count-occurrences s (cdr slist)))))))

(define flatten
  (lambda (slist)
    (foldl 
     (lambda (se acc)
       (if (list? se)
           (append acc (flatten se))
           (append acc (list se))))
     '() slist)))

(define merge
  (lambda (lon1 lon2)
    (cond ((null? lon1) lon2)
          ((null? lon2) lon1)
          ((< (car lon1) (car lon2))
           (cons (car lon1)
                 (merge (cdr lon1) lon2)))
          (else
           (cons (car lon2)
                 (merge lon1 (cdr lon2)))))))