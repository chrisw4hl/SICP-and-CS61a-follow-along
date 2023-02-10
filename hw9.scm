;CS61A HW 9
;SICP 3.16
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

;run-down of (count-pairs ((a b) c d)) === (+ (count-pairs (a b)) (count-pairs (c d)) 1) 
;(+ 3 (count-pairs a) (count-pairs (b)) (count-pairs c) (count-pairs (d)))
;(+ 5)
;key to this problem is having cdrs of pairs pointing to the the same pair in memory as the car of the pair
;i.e. (define a1 (list a b)) (define z1 (cons a1 (cdr a1))) produces a structure with three pairs that counts 4 pairs
;due to both the car and cdr of the top level pair pointing to the same list in memory. Though the overall
;box and pointer diagram for z1 should only have three total pairs.
;returning 7 with three pairs requires (define a1 (list 'a)) (define b1 (cons a1 a1)) (define z1 (cons b1 b1))
;if the equal list is created using symbols '(((a) a) (a) a), it is equal? to z1, but not eq?

;SICP 3.17
;there must be some check to see whether the pairs are reused (eq?) or something
(define (count-pairs-correct x)
  (define (count-pairs-iter x check queue count)
    (cond ((and (null? x) (null? queue)) count) 
          ((null? x) (count-pairs-iter (car queue) check (cdr queue) count))
          ((not (pair? x)) (count-pairs-iter (car queue) check (cdr queue) count))
          ((memq x check) (count-pairs-iter (car queue) check (cdr queue) count))
          (else (count-pairs-iter (car x) (cons x check) (append queue (list (cdr x))) (+ 1 count)))))
  (count-pairs-iter x '() '() 0))

;SICP 3.18 ; solutions for 3.18 and 3.19 do not work with repeated numbers or symbols
(define (cycle? x)
  (define (cycle-help x check)
    (cond ((null? x) #f)
          ((memq (car x) check) #t)
          (else (cycle-help (cdr x) (cons (car x) check)))))
  (cycle-help x '()))

;SICP 3.19- contant space (extra for experts problem) not working
(define (cycle-constant? x)
  (define (cycle-help x check hold)
    (display check)
    (cond ((null? x) #f)
          ((eq? (car x) check) #t)
          ((equal? (car x) check) (cycle-help (cdr x) (car x) (random 10)))
          ((= hold 0) (cycle-help (cdr x) (car x) (random 10)))
          (else (cycle-help (cdr x) check (- hold 1)))))
  (cycle-help (cdr x) 1 (random 10)))



;SICP 3.21
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x) x)
