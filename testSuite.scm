;scheme test suite

((lambda (f p t e) (f p t e)) if #t 3 x) ; => 3 (tests laziness and first class status of if)

((lambda (x x ) x) 1 2) ;=> error about duplicate identifier, not currently working

(let ((x 1) (x 2)) x) ;=> error about duplicate identifier, not currently working

(let ((x 1) (y 2)) (+ x y)) ;=> 3

(letrec ((x 1) (y 2)) (+ x y)) ;=> 3

(let ((x 1)
      (y 2))
    (+ x y)) ;=> 3
    
(letrec ((x y)
         (y 2))
  (+ x y)) ;=> 4   

(let ((rec 5))
  (letrec ((rec 
          (lambda (x) 
              (if (null? x) 
                  'done 
                  (rec (tail x))))))
      (rec '(a b c))))

(letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))))
  (fact 5))
    
(let ((TRUE (lambda (x) (lambda (y) x)))
      (FALSE (lambda (x) (lambda (y) y))))
  (let ((IF (lambda (p) (lambda (t) (lambda (e) (((p t) e)))))))
      (((IF FALSE) (lambda () 1)) (lambda () 2)))) ;=> 2

(let* ((x 1)
       (y (+ x 1)))
    y) ; => 2
    
(let* ((x 1)
       (y (+ x 1))
       (z (+ y 1)))
    z) ;=> 3
    
(let* ((x 1)
       (y (+ x 1))
       (z (+ x y)))
    z) ;=> 3

(let* ((x 1)
       (x (+ x 1))
       (y (+ x 1)))
    y) ;=> 3
    
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
          (let ((y (len-div-6 x)))
            y))))
  (foo 12)) ;=> 2

(let* ((len-div-6 (lambda (len) (* len 6)))
       (foo 
        (lambda (x) 
          (let ((y (len-div-6 x)))
            y))))
  (foo 12)) ;=> 72
  
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (z) 
          (let ((x (len-div-6 z)))
            x))))
  (foo 12)) ;=> 2
  
 (let* ((len-div-6 (lambda (len) (* len 6)))
       (foo 
        (lambda (z) 
          (let ((x (len-div-6 z)))
            x))))
  (foo 12)) ;=> 72

(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
          (let ((x (len-div-6 x)))
            x))))
  (foo 12)) ;=> 2
  
(let* ((len-div-6 (lambda (len) (* len 6)))
       (foo 
        (lambda (x) 
          (let ((x (len-div-6 x)))
            x))))
  (foo 12)) ;=> 72

(let ((len-div-6 (lambda (len) (/ len 6))))
  (let ((foo 
         (lambda (x) 
          (let ((x (len-div-6 x)))
           x))))
    (foo 12))) ;=> 2
    
(let ((len-div-6 (lambda (len) (* len 6))))
  (let ((foo 
         (lambda (x) 
          (let ((x (len-div-6 x)))
           x))))
    (foo 12))) ;=> 2
    
(let ((len-div-6 (lambda (len) (* len 6))))
  (let ((foo 
         (lambda (x) 
          (let ((x (len-div-6 x)))
           x))))
    (foo 12))) ;=> 72
   
(let ((len-div-6 (lambda (len) (* len 6))))
  (let ((foo 
         (lambda (x) 
          (let ((x (len-div-6 x)))
           x))))
    (foo 12))) ;=> 72

(let ((id (lambda (x) x)))
  (let ((foo (lambda (x) 
               (let ((x (id x)))
                 x))))
    (foo 12))) ;=> 12

(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
          (let* ((x (len-div-6 x)))
            x))))
  (foo 12)) ;=> 2
  
(let* ((len-div-6 (lambda (len) (* len 6)))
       (foo 
        (lambda (x) 
          (let* ((x (len-div-6 x)))
            x))))
  (foo 12)) ;=> 72  

(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
		  ((lambda (x) (len-div-6 x)) 
		    x))))
  (foo 12)) ;=> 2
  
(let* ((len-div-6 (lambda (len) (* len 6)))
       (foo 
        (lambda (x) 
		  ((lambda (x) (len-div-6 x)) 
		    x))))
  (foo 12)) ;=> 72 

(let* ((id (lambda (x) x))
       (foo 
	      (lambda (x) 
              ((lambda (x) 
                   (id x))
               x))))
  (foo 12)) ;=> 12

(let ((x 3))
	(let ((y 4))
		x)) ; => 3

((lambda (x)
	(let ((x 3))
		x)) 2) ; => 3

(let* ((id (lambda (x) x))
       (foo 
        (lambda (x) 
          (let ((x (id x)))
            x))))
  (foo 12)) ;=> 12

(let* ((id (lambda (x) x))
        (foo 
            (lambda (x) 
               (let ((x (id x)))
                  x))))
  (foo 12)) ;=> 12

(define id (lambda (x) x))

(define fact 
	(lambda (n)
		(if (zero? n) 1
			(* n (fact (- n 1))))))
            
(fact 5)
			
(define append 
	(lambda (x y)
		(if (null? x) 
            y
			(cons (head x) (append (tail x) y)))))
            
(append '(a b) '(c d))

(define rev
  (lambda (x)
    (if (null? x) '()
        (append (rev (tail x)) (cons (head x) '())))))
        
(rev '(a b c))

(rev '(a))

(define revaux
	(lambda (x acc)
		(if (null? x) 
            acc
			(revaux (tail x) (cons (head x) acc)))))

(define rev 
	(lambda (x) (revaux x '())))
    
(rev '(a b c))
    
(define revaux (lambda (x acc) (if (null? x) acc (revaux (tail x) (cons (head x) acc))))) (define rev (lambda (x) (revaux x (quote ())))) (rev '(a b c)) ;ok
    
; scratch below

((lambda (x y z a) (x (y (z a)))) head tail tail '(a b c))

(define id (lambda (x) x)) (id (head (tail '(a b c))))

(define fact
    (lambda (n)
        (if (zero? n) 1
            (* n (fact (- n 1))))))
          
(fact 5)
            
(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))) (fact 5) ;=> 120

(define foo (lambda (x) (zero? x))) foo (foo 3) ; #<context> #<closure> #f

((lambda () 3)) ;=> 3

(define foo 3) (define fido (lambda (x) (+ x foo))) (fido 2) ;=> 5