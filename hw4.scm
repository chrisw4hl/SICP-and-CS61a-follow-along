;(define (cons x y) (lambda (m) (m x y)))

;(define (car z) (z (lambda (p q) p)))

;(define (cdr z) (z (lambda (p q) q)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;;;SICP 2.10 modifies div-interval to check for intervals that span-zero
(define (div-interval x y)
  (if (or (< (* (lower-bound x) (upper-bound x)) 0) (< (* (lower-bound y) (upper-bound y)) 0))
      '(Error: One or more intervals span zero)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

;;SICP 2.7 adds (lower-bound x) and (upper-bound x)
(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

;;SICP 2.8 adds the sub-interval procedure
(define (sub-interval a b) (make-interval (- (lower-bound a) (lower-bound b))
                                          (- (upper-bound a) (upper-bound b))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (if (< p 1)
      (make-interval (* c (- 1 p)) (* c (+ 1 p)))
      (make-interval (* c (- 1 (/ p 100))) (* c (+ 1 (/ p 100))))))

(define (percent i)
  (if (= (center i) 0)
      '(Error: cannot divide by zero)
      (- (/ (upper-bound i) (center i)) 1)
  ))

(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (reverse list)
  (define (reverse-iter list reversed)
    (if (null? list)
        reversed
        (reverse-iter (cdr list) (cons (car list) reversed))))
(reverse-iter list '()))

(define (same-parity x . y)
  (define (same-parity-helper x y)
    (cond ((empty? y) '())
          ((= (modulo x 2) (remainder (car y) 2)) (cons (car y) (same-parity-helper x (cdr y))))
          (else (same-parity-helper x (cdr y)))


          ))

  (cons x (same-parity-helper x y)))

;;;SICP 2.21
(define  (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

;;;SICP 2.22

(define square (lambda (x) (* x x)))

;each iteration of square-list-iter cons the next item in the list to the left of the previous item. This results in a reverse list
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;Each iteration of this version of square-list-iter keeps the (cons '() (car things)) in the left side of the entire construction.
;The first (cons '() (car things)) is repeated within each successive call of (cons answer (square things)) within the iterative method.
;This results in a list with all levels within the first list car
(define (square-list-iter2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;;;SICP 2.23
(define (for-each proc items)
  (if (null? items)
      '()
      (proc (car items)))
  (if (not (null? items))
      (for-each proc (cdr items))
      (newline) ))

;;;HW4 substitute ***************maybe still add more abstraction?
(define (substitute list oldw neww)
  (define (fix-word list oldw neww)
    (if(equal? (car list) oldw)
        (cons neww (substitute (cdr list) oldw neww))
        (cons (car list) (substitute (cdr list) oldw neww))))

  (cond((null? list) '())
       ((not (list? (car list))) (fix-word list oldw neww)) ;(substitute (cdr list) oldw neww)))
       (else (cons (map (lambda (x) (cond((null? x) '())
                                         ((equal? x oldw) neww)
                                         (else x)))
                        (car list)) (substitute (cdr list) oldw neww)))))
