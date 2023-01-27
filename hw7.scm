(define-class (random-generator base)
    (instance-vars (counter 0))
    (method (number)
        (set! counter (+ 1 counter))
        (random base))
    (method (count)
        counter))

(define-class (coke-machine capacity price)
    (instance-vars (inventory 0)(bank 0))
    (method (fill filler)
            (if (> (+ inventory filler) capacity)
                (set! inventory capacity)
                (set! inventory (+ inventory filler))))
    (method (deposit coins)
            (set! bank (+ bank coins))
            (display "Now there's ")
            (display bank)
            (display " cents in there.\n"))
    (method (coke)
            (if (< bank price)
                (display "Not enough money\n")
                (if (> inventory 0)
                    (begin
                      (set! inventory (- inventory 1))
                      (set! bank (- bank price))
                      (display bank)
                      (display "\n"))
                    (display "Machine Empty\n")))))
