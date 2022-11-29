;**** Homework Week 2 from https://www-inst.eecs.berkeley.edu//~cs61a/reader/nodate-hw.pdf ******
;*****
;**Higher order procedures



;*** Question 1: SICP Exercise 1.31a

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (identity x) x)

(define (inc n) (+ 1 n))

(define (decrement n) (- 1 n))

(define (sum-int a b) (sum identity a increment b))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial a) (product identity 1 increment a))

(define (pi-approx-num n) (product (lambda (x) (* (* 2 x) (* 2 x))) 2 increment n))

(define (pi-approx-den n) (product (lambda (x) (* x x)) 3 (lambda (x) (+ x 2)) (+ (* 2 n) 1)))

(define (pi-approx n) (* (* 8 (* 2 (+ n 1))) (/ (pi-approx-num n) (pi-approx-den n))))

;;;(define (factorial n) (product (lambda (x) x) 1 (lambda (x) (+ 1 x)) n))

;;;Abelson 1.32a

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))


;;;following ripped from pg 50 of Abelson & Sussman

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> ((lambda (n) (* n n)) test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;;;1.33 Abelson


(define (filter-accumulate combiner null-value filter term a next b)
  (cond ((> a b)  null-value)
	((and (<= a b) (filter a)) (combiner (term a) (filter-accumulate combiner null-value filter term (next a) next b)))
	(else (combiner null-value
		(filter-accumulate combiner null-value filter term (next a) next b)))))

;;;(combiner (term a) (filter-accumulate combiner null-value filter term a next b)


(define (sum-filt a b) (filter-accumulate + 0 prime? identity a increment b))

(define (sum-sq-prime a b) (filter-accumulate + 0 prime? (lambda (x) (* x x)) a increment b))

;;;

(define tolerance 0.00001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2))
           tolerance))
	(define	(try guess)
  		(let ((next (f guess)))
			(if (close-enough? guess next)
			next
			(try next))))
(try first-guess))

(define (deriv g) (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (fixed-point-of-transform g transform guess)
	(fixed-point (transform g) guess))

(define ( newtons-method g guess)
  
	(fixed-point (newton-transform g) guess))

;;x^3 + a*x^2 +b*x +c

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (cubic a b c)
	(newtons-method (lambda (x) (- 0 (cube x) (* a (square x)) (* b x) c)) 1.0))

(cubic 3 2 1)


(define (double p)
	(lambda (x) (p  (p x))))

((double (lambda (x) (+ x 1))) 1)

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)

    (if (> n 0)

    (lambda (x) (f (repeated f (- n 1))))

    (lambda (x) (f x))))

 

;;;((repeated square 2) 5) 
