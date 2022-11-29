

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C) '(Z Z) ))

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 54) )


(define (best-total hand)
  (best-total-iter hand 0 0))

(define (best-total-iter hand count aces)
  (cond ((empty? hand) (add-ace count aces))
	((equal? 'a (trifirst hand)) (best-total-iter (bf hand) count (+ 1 aces)))
	((member? (trifirst hand) '(q k j)) (best-total-iter (bf hand) (+ count 10) aces))
  ((member? (trifirst hand) '(Z)) (best-total-iter (bf hand) (+ count 11) aces))
	((= (trifirst hand) 1) (best-total-iter (bf hand) (+ count 10) aces))
	(else (best-total-iter (bf hand) (+ count (trifirst hand)) aces))

	))

(define (add-ace count aces)
  (cond ((= aces 0) count)
	((>= count 11) (add-ace (+ count 1) (- aces 1)))
	(else (add-ace (+ count 11) (- aces 1)))))

(define (trifirst x) (first (first (first x))))

(define (stop-at-17 customer-hand-so-far dealer-up-card)
  (cond ((< (best-total customer-hand-so-far) 17) #t)
	(else #f)))

(define (play-n strategy n)
  (play-n-iter strategy n 0))

(define (play-n-iter strategy n count)
  (if (= n 0)
      count
      (play-n-iter strategy (- n 1) (+ count (twenty-one strategy)))))

(define (dealer-sensitive customer-hand-so-far dealer-up-card)
  (cond ((and (member? (first dealer-up-card) '(7 8 9 10 j q k a)) (< (best-total customer-hand-so-far) 17)) #t)
	((and (< (best-total customer-hand-so-far) 12) (member? (first dealer-up-card) '(2 3 4 5 6))) #t)
	(else #f)))

(define (stop-at n)
  (lambda (x y) (cond ((< (best-total x) n) #t)
		      (else #f))))

;;;;screwed something up
(define (valentine customer-hand-so-far dealer-up-card)
  (cond ((and (member? 'h (convert customer-hand-so-far)) (< (best-total customer-hand-so-far) 19)) #t)
	((< (best-total customer-hand-so-far) 17) #t)
	(else #f)))

(define (convert customer-hand-so-far)
  (converter customer-hand-so-far '()))

;;;;;error checking first of ace against integer
(define (converter customer-hand-so-far store)
  (cond ((empty? customer-hand-so-far) store)
        ((member? (first (first customer-hand-so-far)) '(a k q j)) (converter (bf customer-hand-so-far) (se store (bf (first customer-hand-so-far)))))
        ((= (first (first customer-hand-so-far)) 1) (converter (bf customer-hand-so-far) (se store (bf (bf (first customer-hand-so-far))))))
        ((member? (first (first customer-hand-so-far)) '(Z)) (converter (bf customer-hand-so-far) (se store)))
        ((converter (bf customer-hand-so-far) (se store (bf (first customer-hand-so-far)))))))

;not right yet
(define (suit-strategy suit do-strat no-strat)
  (lambda (customer-hand-so-far dealer-up-card) (cond ((and (member? suit (convert customer-hand-so-far)) (do-strat customer-hand-so-far dealer-up-card)) #t)
						      ((and (not (member? suit (convert customer-hand-so-far))) (no-strat customer-hand-so-far dealer-up-card)) #t)
						      (else #f))))


(define valentine2
  (suit-strategy 'h (stop-at 19) (stop-at 17)))

(define (majority strat1 strat2 strat3)
  (lambda (x y) (if (or (and (strat1 x y) (strat2 x y)) (and (strat2 x y) (strat3 x y)) (and (strat1 x y) (strat3 x y)))
		    #t
		    #f)))

;;in dev
(define (reckless strat)
  (lambda (x y) (cond ((and (strat x y) #t) #t)
                      ((and (strat (bl x) y) #t) #t)
                      (else #f)
                      )))
