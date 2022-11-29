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

(define (list-test x . y)
  ;x
  ;(car y)
  (cdr y)
  ;y
  )
