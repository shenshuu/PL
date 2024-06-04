(define path
  (lambda (n bst)
    (cond ((or (null? bst) (= n (car bst))) '())
          ((< n (car bst)) (cons 'left (path n (cadr bst))))
          ((> n (car bst)) (cons 'right (path n (caddr bst)))))))

(path 17 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))

(define merge
  (lambda (pred lon1 lon2)
    (cond ((null? lon1) lon2)
          ((null? lon2) lon1)
          ((pred (car lon1) (car lon2))
           (cons (car lon1)
                 (merge pred (cdr lon1) lon2)))
          (else
           (cons (car lon2)
                 (merge pred lon1 (cdr lon2)))))))

(define sort
  (lambda (predicate lon)
    (if (<= (length lon) 1)
        lon
        (let* ((mid (floor (/ (length lon) 2)))
               (xs (sort predicate (take lon mid)))
               (ys (sort predicate (drop lon mid))))
          (merge predicate xs ys)))))
               
(sort > '(8 2 5 2 3))