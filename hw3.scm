;;SICP Exercise 1.16: Sqrt Iter log(n)

(define (square-iter b n count)
  (if (< n 1)
      count
      (square-iter b
		   (- n 1)
		   (* b count) )))

(define (square x) (* x x))

(define (fast-square-iter b n count)
  (cond ((= n 0) count)
	((even? n) (fast-square-iter b (/ n 2) (* (square b) count)))
	((= n 1) (fast-square-iter b (- n 1) count))
	(else (fast-square-iter b (- n 1) (* b count)))))


;;SICP Exercise 1.37: Infinite Continued Fraction

(define (inf-frac N D k count)
  (if (< k 1)
      1
      (/ (N count) (+ (D count) (inf-frac N D (- k 1) (+ 1 count)) )) ))

(define (run-frac n)
  (inf-frac (lambda (i) 1.0) (lambda (i) 1.0) n))


;definitely wrong
(define (inf-frac-iter N D k countnum countd)
  (if (< k 1)
      (/ countnum countd)
      (inf-frac-iter N
		     D
		     (- k 1)
		     countnum
		     (+ cound (/ (N k) (+ (D k) (/ (N (+ k 1)) (D (+ k 1)))))))))

;;CISP 1.38

(define (e-frac k)
  (inf-frac (lambda (i) 1.0) (Di) k 1))

(define (Di)
  (lambda (i) (cond ((= i 2) 2)
		    ((= i 5) 4)
		    ((= i 8) 6)
		    ((= i 11) 8)
		    (else 1))))

(define (next-perf n)
  (if (sum-of-factors n 1 0)
      n
      (next-perf (+ n 1))))

(define (sum-of-factors n count check)
  (cond ((and (= n check) (> count (/ n 2)))  #t)
	((or (> count  (/ n 2)) (= n count)) #f)
	((= (remainder n count) 0) (sum-of-factors n (+ count 1) (+ check count)))
	(else (sum-of-factors n (+ count 1) check))))

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ;((or (< amount 0) (= kinds-of-coins 0)) 0)
	((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination
			 kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))
