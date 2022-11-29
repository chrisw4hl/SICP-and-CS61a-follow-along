;;;;;;;Lab Week 2 practice problems for UCB CS61A self study

(define (dupls-removed sent)
	(if (empty? sent)
	    '()
	    (if (member? (first sent) (bf sent))
		(dupls-removed (bf sent))
		(se (first sent) (dupls-removed (bf  sent))) )))



(define (substitute sent oldw neww)
  (cond ((empty? sent)
	 '())
	((
      

