;; Scheme calculator -- evaluate simple expressions

#|
To run the calculator program, load this file into STk with (load "hw1.scm")
and call the calc function: (calc)

Try out some simple expressions with addition and subtraction
to get a feel for the program. It is essentially a subset of
Scheme with only the +, -, *, and / procedures supported.

For example:
STk> (load "hw1.scm")
okay
STk> (calc)
calc: (+ (+ 2 3) (- 5 6) 4)

You have to implement *-helper, ^-helper, and /-helper for this assignment.
|#

;; The read-eval-print loop:
(define (calc)
  (display "calc: ")
  (flush)
  (print (calc-eval (read)))
  (calc))

;; Evaluate an expression:
(define (calc-eval exp)
  (cond ((number? exp) exp)
	((list? exp) (calc-apply (first exp) (every calc-eval (bf exp))))
	(else (error "Calc: bad expression:" exp))))


			   (else (- (first args) (accumulate + 0 (bf args))))))
	((eq? fn '*) (if (= 2 (count args))
			 (apply *-helper args)
			 (error "Calc: * takes 2 args")))
	((eq? fn '/) (if (= 2 (count args))
			 (apply /-helper args)
			 (error "Calc: / takes 2 args")))
	((eq? fn '^) (if (= 2 (count args))
			 (apply ^-helper args)
			 (error "Calc: ^ takes 2 args")))
	((eq? fn '**) (if (= 2 (count args))
			 (apply karatsuba args)
			 (error "Calc: Fast Multiplication takes 2 args")))
	(else (error "Calc: bad operator:" fn))))

;;; Homework starts here!
;; Fill in these procedures

;; Section 1
;; Question 1
(define (*-helper a b)
  (cond ((or (empty? a) (empty? b))
      '())
	( (= b 0)  0)
	(else (+ a (*-helper a (- b 1))))))
 
;; Question 2
(define (^-helper a b)
  (cond ((or (empty? a) (empty? b))
      '())
	( (= b 0)  1)
	(else (*-helper a (^-helper a (- b 1))))))
    
;; Question 3
(define (/-helper a b)
  (/-iter a b 0))

(define (/-iter a b count)
    (cond ((< (- a b) 0) (se  count a))
	  ((= (- a b) 0) (se (+ 1 count) 0))
	  (else (/-iter (- a b) b (+ count 1)))))
    
;; Question 4
(define (karatsuba a b)
  (define n (karatsuba-length a 0))
  (karatsuba-helper a b n))

(define (karatsuba-length a c)
  ;;find length of integers, assuming length a == length b from prompt
  (if (empty? a) c (karatsuba-length (bf a) (+ c 1))))

(define (karatsuba-helper a b n)
  (if  (<= n 1) (* a b) (karatsuba-recur a b n)))
     
(define (karatsuba-recur a b n)
  ;;only works with numbers same length, and length being a power of 2. Would be nice to generalize for odd length numbers
  (define m2 (first (/-helper n 2)))
  (define higha (fsplit a m2))
  (define lowa (bsplit a m2))
  (define highb (fsplit b m2))
  (define lowb (bsplit b m2))
  (define z2 (karatsuba higha highb))
  (define z0 (karatsuba lowa lowb))
  (define z1 (karatsuba (+ lowa higha) (+ lowb highb)))
  (+ (* (expt 10 (* m2 2)) z2) (* (expt 10 m2) (- z1 (+ z2 z0))) z0))

(define (fsplit a n)
  (if (= (karatsuba-length a 0) n) a (fsplit (bl a) n)))

(define (bsplit a n)
  (if (= (karatsuba-length a 0) n) a (bsplit (bf a) n)))

 ;; Section 2: Recursion
 ;; Question 1
(define (squares nums)
  (if (empty? nums) '() (se (* (first nums) (first nums)) (squares (bf nums)))))
 
 ;; Question 2
(define (switch sent)
  (switch-helper sent 0))

(define (switch-helper sent count)
  (cond ((empty? sent) '()	((member? (first sent) '(i me)) (se 'you (switch-helper (bf sent) (+ 1 count))))
	((member? (first sent) '(you)) (if(= count 0) (se 'i (switch (bf sent)))(se 'me (switch-helper (bf sent) (+ 1 count) ))))
	(else (se (first sent) (switch-helper (bf sent) (+ 1 count)))))
  
 )
 
 ;; Question 3a
(define (first-streak strk)
  (first-streak-helper strk 'new 0)

  )

(define (first-streak-helper strk str count)
  (if (equal? str 'new) (first-streak-helper (bf strk) (first strk) (+ 1 count))
	(if (equal? (first strk) str) (first-streak-helper (bf strk) str (+ 1 count)) count)))

 ;; Question 3b
(define (best-streak strk)
  (best-streak-helper strk 'new 0 0)
 )
(define (best-streak-helper strk str count longest)
  (cond ((empty? strk) longest)
	((equal? str 'new) (best-streak-helper (bf strk) (first strk) (+ 1 count) (+ 1 longest)))
	((equal? (first strk) str) (if (> (+ count 1) longest) (best-streak-helper (bf strk) str (+ 1 count) (+ 1 longest))
				       (best-streak-helper (bf strk) str (+ 1 count) longest)))
	(else (best-streak-helper (bf strk) (first strk) 1 longest)))
  )

 
(define (ordered? sent)
  (cond ((empty? sent) #t)
	((empty? (bf sent))(ordered? (bf sent)))
	((>= (first (bf sent)) (first sent)) (ordered? (bf sent)))
	(else #f)))

(define (ends-e sent)
  (cond ((empty? sent) '())
	((equal? (last (first sent)) 'e) (se (first sent) (ends-e (bf sent))))
	(else (ends-e (bf sent)))))

(define (test x)
  (test x))

 ;; Section 3: Higher Order Procedures
 
 ;; Question 1:
 (define (my-every fn sent)
   (error "Not Implemented!") ;; Your code goes here
 )
 
 ;; Question 2:
 (define (repeated fn ntimes)
   (error "Not Implemented!") ;; Your code goes here
 )
 
 ;; Question 3a:
 (define (insert new-num sorted-sent)
    (error "Not Implemented!") ;; Your code goes here
 )
 
 ;; Question 3b:
 (define (insertion-sort unsorted-sent)
	(define (insertion-sort-helper unsorted-sent sorted-sent)
          (error "Not Implemented!") ;; Your code goes here
	)
    (insertion-sort-helper unsorted-sent '())

  )
  
 
 ;; Question 3c:
 (define (hof-insertion-sort comparator sent)
   (error "Not Implemented!") ;; Your code goes here
 ) 
 

