(define append
    (lambda (x y)
        (if (null? x) y
            (cons (head x) (append (tail x) y)))))
            
(define rev
    (lambda (x)
        (if (null? x) '()
            (append (rev (tail x)) (list (head x))))))
            
(rev '(a b c))

(define y
  (lambda (f)
    ((lambda (x)
      (f (x x)))
     (lambda (x)
       (f (x x))))))
       
(define facty
  (y 
    (lambda (fact)
      (lambda (n)
        (if (zero? n) 1
            (* n (fact (- n 1))))))))
            
(facty 5)        