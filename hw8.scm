;;following implementation of make-account from SICP, modified for Exercise 3.3 and 3.4
(define (make-account balance . password)

  (define password-attempts 0) ;initialize the local variable for password-attempts

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (security amount)
    (set! password-attempts (+ password-attempts 1))
    (if (> password-attempts 7)
        (call-the-cops)
        "incorrect password"))

  (define (call-the-cops)
    "calling-the-police")

  (define (dispatch m . n)
    (if (null? password) 
        (cond ((eq? m 'withdraw) withdraw) ;if there is no pass word set, perform this cond clause
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request: MAKE-ACCOUNT"
                       m)))
        (cond ((and (member? m password) (eq? (car n) 'withdraw)) (begin (set! password-attempts 0) withdraw)) ;if a password is set during initialization, perform this cond clause
              ((and (member? m password) (eq? (car n)  'deposit)) (begin (set! password-attempts 0) deposit)) ;also reset the password attempts if the password is correct
              ((and (member? m password) (eq? (car n) 'reset)) (begin 
                                                                   (set! password-attempts 0)
                                                                   (set! password (append (cdr n) password))))
              ((eq? m (car password)) (error "Unknown request: MAKE-ACCOUNT"
                         n))
              (else security)))) ;if the password is incorrect, call the security procedure
  dispatch)

;SICP 3.7

(define (make-joint acc orig-pass new-pass)
  (acc orig-pass 'reset new-pass)
  acc)

;SICP 3.8 

(define (eval-order . arg)
  (let ((x 0))
    (lambda (arg)
        (if (= arg 0) (begin (set! x 1) 0)
            x))))

(define h (eval-order))
(define g (eval-order))

(define (counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ 1 count))
      count)))

;SICP 3.10
;testing differences in implementation for environment model analysis
(define (make-withdraw bal)
  (lambda (amount)
    (if (>= bal amount)
      (begin (set! bal (- bal amount))
             bal)
      "Insufficient funds")))

(define (make-withdraw-let init)
  (let ((bal init))
    (lambda (amount)
      (if (>= bal amount)
        (begin (set! bal (- bal amount))
               bal)
        "Insufficient funds"))))

;;SICP 3.10 and 3.11 drawn out on paper
;;Immportant points are that the new version of make-account in 3.10 with the added let creates a new procedure
;;bound to the frame created when (W1 50) is invoked. The behavior of both implementations is the same.
;;***may need some clarification for 3.10
;;3.11 creates a local frame with local procedures withdraw, deposit, and dispatch defined in the local frame.
;;The local dispatch procedure is linked to the global value of the account object.
;;When called, the account object returns the correct local procedure from dispatch, and is evaluated with the specified amount.
;;separate account objects create new local frames with additional withdraw, deposit, dispatch, and balance variables.
;;The account objects are linked to their respective local dispatch procedures.
