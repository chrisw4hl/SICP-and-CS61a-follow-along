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

;SICP 3.8 (not working)

(define (eval-order arg)
    (if (= arg 0) (begin (set! arg 1) 0)
        arg))
