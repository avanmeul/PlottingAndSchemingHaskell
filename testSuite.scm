;scheme test suite

;ok
(let ((TRUE (lambda (x) (lambda (y) x)))
      (FALSE (lambda (x) (lambda (y) y))))
  (let ((IF (lambda (p) (lambda (t) (lambda (e) (((p t) e)))))))
      (((IF FALSE) (lambda () 1)) (lambda () 2))))
;=> 2

(let* ((x 1)
       (y (+ x 1)))
    y) ; => 2
    
   
(let* ((x 1)
       (y (+ x 1))
       (z (+ x y)))
    z) ;=> 3
    
;ok
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
          (let ((y (len-div-6 x)))
            y))))
  (foo 12)) ;=> 2

;ok
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (z) 
          (let ((x (len-div-6 z)))
            x))))
  (foo 12)) ;=> 2

;given that this works, it suggests the problem (after alpha renaming to x) is a lookup failure
;bad
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
          (let ((x (len-div-6 x)))
            x))))
  (foo 12)) ;=> 2

;question:  is x in the environment; i.e. is this a lookup failure, or an environment making error?

;bad
(let ((len-div-6 (lambda (len) (/ len 6))))
  (let ((foo 
         (lambda (x) 
          (let ((x (len-div-6 x)))
           x))))
    (foo 12))) ;=> 2
   
;good   
(let ((len-div-6 (lambda (len) (* len 6))))
  (let ((foo 
         (lambda (x) 
          (let ((x (len-div-6 x)))
           x))))
    (foo 12))) ;=> 72

;bad (stack overflow), good now
(let ((id (lambda (x) x)))
  (let ((foo (lambda (x) 
               (let ((x (id x)))
                 x))))
    (foo 12))) ;=> 12

;bad
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
          (let* ((x (len-div-6 x)))
            x))))
  (foo 12)) ;=> 2

;ok
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
		  ((lambda (x) (len-div-6 x)) 
		    x))))
  (foo 12)) ;=> 2

;ok
(let* ((id (lambda (x) x))
       (foo 
	      (lambda (x) 
              ((lambda (x) 
                   (id x))
               x))))
  (foo 12)) ;=> 12

; this works
(let ((x 3))
	(let ((y 4))
		x))
; => 3

((lambda (x)
	(let ((x 3))
		x)) 2)
; => 3

;this doesn't work:
(let* ((id (lambda (x) x))
       (foo 
        (lambda (x) 
          (let ((x (id x)))
            x))))
  (foo 12)) ;=> 12

;doesn't work (works in Dr. Racket)
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
			
(define append 
	(lambda (x y)
		(if (null? x) y
			(cons (car x) (append (cdr x) y)))))

(define rev
  (lambda (x)
    (if (null? x) '()
        (append (rev (cdr x)) (list (car x))))))

;doesn't work, works now
(rev '(a))

(define revaux
	(lambda (x acc)
		(if (null? x) acc
			(revaux (cdr x) (cons (car x) acc)))))

(define rev 
	(lambda (x) (revaux x '())))
    
(define revaux (lambda (x acc) (if (null? x) acc (revaux (tail x) (cons (head x) acc))))) (define rev (lambda (x) (revaux x (quote ())))) (rev '(a b c)) ;ok
    
; scratch below

((lambda (x y z a) (x (y (z a)))) head tail tail '(a b c))

(define id (lambda (x) x)) (id (head (tail '(a b c))))

(define fact
    (lambda (n)
        (if (zero? n) 1
            (* n (fact (- n 1))))))
            
(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))) (fact 5)

; [
; ScmError {errCaller = "evalHeaps", errMessage = "failed in evaluating ObjCons (ScmCons {scmCar = ObjSymbol \"fact\", scmCdr = ObjCons (ScmCons {scmCar = ObjImmediate (ImmInt 1), scmCdr = ObjImmediate (ImmSym \"()\")})})"},
; ScmError {errCaller = "scmTimes", errMessage = "argument caused failure: ObjCons (ScmCons {scmCar = ObjSymbol \"fact\", scmCdr = ObjCons (ScmCons {scmCar = ObjCons (ScmCons {scmCar = ObjSymbol \"-\", scmCdr = ObjCons (ScmCons {scmCar = ObjSymbol \"n\", scmCdr = ObjCons (ScmCons {scmCar = ObjImmediate (ImmInt 1), scmCdr = ObjImmediate (ImmSym \"()\")})})}), scmCdr = ObjImmediate (ImmSym \"()\")})})"},
; ScmError {errCaller = "eval", errMessage = "bad function "},
; ScmError {errCaller = "eval", errMessage = "symbol lookup failed: fact, global env = [(\"quote\",ObjPrimitive (ScmPrimitive {priName = \"quote\", priFunction = <function>})),(\"head\",ObjPrimitive (ScmPrimitive {priName = \"head\", priFunction = <function>})),(\"tail\",ObjPrimitive (ScmPrimitive {priName = \"tail\", priFunction = <function>})),(\"define\",ObjPrimitive (ScmPrimitive {priName = \"define\", priFunction = <function>})),(\"if\",ObjPrimitive (ScmPrimitive {priName = \"if\", priFunction = <function>})),(\"zero?\",ObjPrimitive (ScmPrimitive {priName = \"zero?\", priFunction = <function>})),(\"+\",ObjPrimitive (ScmPrimitive {priName = \"+\", priFunction = <function>})),(\"-\",ObjPrimitive (ScmPrimitive {priName = \"-\", priFunction = <function>})),(\"*\",ObjPrimitive (ScmPrimitive {priName = \"*\", priFunction = <function>}))]"}]
; [ScmError {errCaller = "evalHeaps", errMessage = "failed in evaluating ObjCons (ScmCons {scmCar = ObjSymbol \"fact\", scmCdr = ObjCons (ScmCons {scmCar = ObjImmediate (ImmInt 1), scmCdr = ObjImmediate (ImmSym \"()\")})})"},
; ScmError {errCaller = "scmTimes", errMessage = "argument caused failure: ObjCons (ScmCons {scmCar = ObjSymbol \"fact\", scmCdr = ObjCons (ScmCons {scmCar = ObjCons (ScmCons {scmCar = ObjSymbol \"-\", scmCdr = ObjCons (ScmCons {scmCar = ObjSymbol \"n\", scmCdr = ObjCons (ScmCons {scmCar = ObjImmediate (ImmInt 1), scmCdr = ObjImmediate (ImmSym \"()\")})})}), scmCdr = ObjImmediate (ImmSym \"()\")})})"},
; ScmError {errCaller = "eval", errMessage = "bad function "},
; ScmError {errCaller = "eval", errMessage = "symbol lookup failed: fact, global env = 
; [(\"quote\",ObjPrimitive (ScmPrimitive {priName = \"quote\", priFunction = <function>}))
; ,(\"head\",ObjPrimitive (ScmPrimitive {priName = \"head\", priFunction = <function>}))
; ,(\"tail\",ObjPrimitive (ScmPrimitive {priName = \"tail\", priFunction = <function>})),(\"define\",ObjPrimitive (ScmPrimitive {priName = \"define\", priFunction = <function>})),(\"if\",ObjPrimitive (ScmPrimitive {priName = \"if\", priFunction = <function>})),(\"zero?\",ObjPrimitive (ScmPrimitive {priName = \"zero?\", priFunction = <function>})),(\"+\",ObjPrimitive (ScmPrimitive {priName = \"+\", priFunction = <function>})),(\"-\",ObjPrimitive (ScmPrimitive {priName = \"-\", priFunction = <function>}))
; ,(\"*\",ObjPrimitive (ScmPrimitive {priName = \"*\", priFunction = <function>}))]"}]


(define foo (lambda (x) (zero? x))) foo (foo 3)

(define foo (lambda (x) 

something to try:  ((lambda () 3)) ;to test null arg list

; [
; ScmError {errCaller = "evalHeaps", errMessage = "failed in evaluating ObjCons (ScmCons {scmCar = ObjSymbol \"foo\", scmCdr = ObjCons (ScmCons {scmCar = ObjImmediate (ImmInt 3), scmCdr = ObjImmediate (ImmSym \"()\")})})"},
; ScmError {errCaller = "scmZero", errMessage = "bad argument: Just (ObjSymbol \"x\")"}
; ]       

(define foo 3) (define fido (lambda (x) (+ x foo))) (fido 2)

