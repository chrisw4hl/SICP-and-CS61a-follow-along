(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (add x y)
  (apply-generic 'add x y))
(define (sub x y)
  (apply-generic 'sub x y))
(define (mul x y) 
  (apply-generic 'mul x y))
(define (div x y) 
  (apply-generic 'div x y))
(define (equ? x y)
  (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  (define (tag x) 
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda(x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda(x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))
  'done)

;(define (apply-generic op arg) (arg op))

;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;        (apply proc (map contents args))
;        (error "No method for these types: APPLY-GENERIC"
;               (list op type-tags))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons(/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda(x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda(x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda(x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda(x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda(n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda(x y) (and (eq? (numer x) (numer y)) (eq? (denom x) (denom y)))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; internal procedures
  (define (install-polar-package)
    (define (tag z) (attach-tag 'polar z))
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (real-part z) (* (magnitude z) (cos (angle z))))
    (define (imag-part z) (* (magnitude z) (sin (angle z))))
    (define (make-from-mag-ang r a) (cons r a))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a)))))
  (install-polar-package)
  
  (define (install-rectangular-package)
    (define (tag z) (attach-tag 'rectangular z))
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (magnitude z) (sqrt (+ (square (real-part z))
                                   (square (imag-part z)))))
    (define (angle z) (atan (imag-part z) (real-part z)))
    (define (make-from-real-imag x y) (cons x y))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular 
         (lambda (x y) (tag (make-from-real-imag x y)))))
  (install-rectangular-package)

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (real-part z1)
    (apply-generic 'real-part z1))  
  (define (imag-part z1)
    (apply-generic 'imag-part z1))
  (define (magnitude z1)
    (apply-generic 'magnitude z1))
  (define (angle z1)
    (apply-generic 'angle z1))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (and (equal? (real-part z1) (real-part z2))
                            (equal? (imag-part z1) (imag-part z2)))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))


;2.75

;(define (make-from-mag-ang r a)
;  (define (dispatch op)
;    (cond ((eq? op 'real-part) (* (cos a) r))
;          ((eq? op 'imag-part) (* (sin a) r))
;          ((eq? op 'magnitude) r)
;          ((eq? op 'angle) a)
;          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op)))))

;2.76
;explicit dispatch will require the selectors to know about each data type, and must be modified every time anew type is added
;data-directed style will only require that the correct procedures be implemented in the operations table, corresponding to the new data type
;then the apply-generic procedure will know how to operate on the added data types
;message-passing style only requires the added procedure with its own internal dispatch procedrue. Nothing else needs to be modified in the external program.

;2.77
;before the addition of the data table contents instructions on how to handle 'complex objects
;the apply-generic procedure hits an error because there is no procedure defined for 'real-part 'complex
;the apply-generic procedure can still add the data objects, because there is a defineed procedure to handle
;adding the data object '(complex complex), which is passed when 'add is called for the complex number objects
;by putting the procedure directly in the table for 'complex data objects, the (apply-generic 'real-part)
;procedure can be called to the data object with the complex tag stripped off, so the correct 'rectangular or
;'polar procedures can be called.

;2.81
;items of the same type, if no function is stated for them, will procede down to the porion of apply-generic which checks for coercion table entires
;apply-generic will then return the error message that there are no method defined for the types in the type table
;With Louis' coercion procedures installed, the other error message inside the coercion check will throw, saying there are no methods defined for the types in the outer check
;the self-coercion is not necessarily needed, except to avoid specific error messages. The self coercion should not be doing anything substantial

;2.83
;change apply-generic with coercion to avoid self-coercion. Skipped because my build of STK doesn't seem to have put-coercion and get-coercion
