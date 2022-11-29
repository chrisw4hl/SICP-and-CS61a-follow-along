;;general iterative-improve function SICP 1.46
(define (iterative-improve check process guess)
  (lambda (x)  (if (check guess x)
		    guess
		    ((iterative-improve check process (process guess x)) x))))

(define (repeated f n)
  (if (> n 1)
      (compose f  (repeated f (- n 1)))
      (compose f (lambda (x) x))))
		  

(define (inc x) (+ 1 x))

(define (square x) (* x x))


(define (compose f g) (lambda (x)  (f (g x))))

((compose square inc) 6)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) .001))

(define (sqrt-process guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

(define sqrt1 (iterative-improve good-enough? sqrt-process 1.0))

(define (fixed-point-check next f)
  (< (abs (- next (f next))) .00001))

(define (fixed-point-process x f)
  (f x))

(define fixed-point-meth (iterative-improve fixed-point-check fixed-point-process 0))

(define test (fixed-point-meth (lambda (x) (+ (sin x) (cos x)))))


;;:HW2 Section 2.


(define (every1 proc sent)
  (if (empty? sent)
      '()
      (se (proc (first sent)) (every proc (bf sent)))))

;;HW2 Section 3

((lambda (a b)
   ((lambda (square)
      (+ (square a) (square b)))
    (lambda (x) (* x x))))
 3 4)


; Can't figure out extra for experts. Iteration?					;
;((lambda (x)
;   (if (< x 1)
;       1
;       (lambda (x) (* 

