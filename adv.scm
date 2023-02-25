;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

(define-class (place name)
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '()))
  (parent (basic-object name))
  (method (type) 'place)
  (method (place?) #t)
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	  '()                     ;; nothing in that direction
	  (cdr pair))))           ;; return the place object
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))
    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	(error "Person already in this place" (list name new-person)))
    (set! people (cons new-person people))
    (for-each (lambda (proc) (proc)) entry-procs)
    (for-each (lambda (obj) (ask obj 'notice obj)) (cdr people))
    'appeared)
  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)
  (method (may-enter? person)
          #t)
  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)

  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared) )

(define-class (locked-place name)
    (instance-vars
      (locked #t))
    (parent (place name))
    (method (type) 'locked-place)
    (method (may-enter? person)
        (not locked))
    (method (unlock)
        (set! locked #f)
        (announce-unlock name)))

(define-class (garage name)
    (class-vars (ticket-number 0))
    (instance-vars (table (make-table)))
    (parent (place name))
    (method (park car-item)
        (if (memq car-item (ask self 'things))
            (begin
                (set! ticket-number (+ 1 ticket-number))
                (insert! ticket-number car-item table)
                (ask (ask car-item 'possessor) 'take (instantiate ticket 'TICKET ticket-number self))
                (ask (ask car-item 'possessor) 'lose car-item))
            (error "car not in garage")))
    (method (unpark ticket)
        (if (eq? (ask ticket 'name) 'TICKET)
          (begin
            (if (lookup (ask ticket 'serial) table)
                (begin
                (let ((car-item (lookup (ask ticket 'serial) table)))
                  (ask (ask ticket 'possessor) 'take car-item)
                  (ask (ask ticket 'possessor) 'lose ticket)
                  (insert! (ask ticket 'serial) #f table)))
                (error "ticket not associated with a car")))
          (error "not a ticket"))))

(define-class (hotspot name password)
    (instance-vars
        (connected-laptops '()))
    (parent (place name))
    (method (connect laptop user-password)
            (if (eq? (ask laptop 'type) 'laptop)
              (if (memq laptop (ask self 'things)) 
                (if (eq? password user-password)
                  (set! connected-laptops (cons laptop connected-laptops))
                  (error "password is incorret for" name))
                (error "laptop trying to connect is not within range of" name))
              (error "item trying to connect is not a laptop" laptop)))
    (method (disconnect laptop)
            (set! connected-laptops 
              (filter (lambda (x) (not (eq? laptop x))) 
                                            connected-laptops)))
    (method (surf laptop website)
            (if (memq laptop connected-laptops)
              (system (string-append "lynx " website))
              (error "laptop not connected to network")))
    (method (gone laptop)
        (if (eq? (ask laptop 'type) 'laptop)
                (begin
                    (ask self 'disconnect laptop)
                    (usual 'gone laptop))
                (usual 'gone laptop))))

          
(define-class (person name place)
  (instance-vars
   (possessions '())
   (saying ""))
  (initialize
   (ask place 'enter self)
   (ask self 'put 'strength 50)
   (ask self 'put 'money 100))
  (parent (basic-object name))
  (method (type) 'person)
  (method (person?) #t)
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  (method (take thing)
    (cond ((not (thing? thing)) (error "Not a thing" thing))
	  ((not (memq thing (ask place 'things)))
	   (error "Thing taken not at this place"
		  (list (ask place 'name) thing)))
	  ((memq thing possessions) (error "You already have it!"))
	  (else
	   (announce-take name thing)
	   (set! possessions (cons thing possessions))
	       
	   ;; If somebody already has this object...
	   (for-each
	    (lambda (pers)
	      (if (and (not (eq? pers self)) ; ignore myself
		       (memq thing (ask pers 'possessions)))
		  (begin
		   (ask pers 'lose thing)
		   (have-fit pers))))
	    (ask place 'people))
	       
	   (ask thing 'change-possessor self)
	   'taken)))

  (method (take-all place)
    (map (lambda (x) (ask self 'take x)) 
           (filter (lambda (x) (null? (ask x 'possessor)))
                   (ask place 'things))))

  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))
  (method (go direction)
    (let ((new-place (ask place 'look-in direction)))
      (cond ((null? new-place)
	     (error "Can't go" direction))
            ((not (ask new-place 'may-enter? self))
             (error "locked" direction))
	    (else
	     (ask place 'exit self)
	     (announce-move name place new-place)
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))
	      possessions)
	     (set! place new-place)
	     (ask new-place 'enter self)))))
  (method (go-directly-to new-place)
    (ask place 'exit self)
    (announce-move name place new-place)
    (for-each
      (lambda (p)
        (ask place 'gone p)
        (ask new-place 'appear p))
      possessions)
    (set! place new-place)
    (ask new-place 'enter self))
  (method (get-money num)
    (ask self 'put 'money (+ (ask self 'money) num)))
  (method (pay-amount num)
    (if (> num (ask self 'money))
      #f
      (ask self 'put 'money (- (ask self 'money) num))))
  (method (buy order)
    (ask place 'sell self order))
  (method (eat food)
    (if (and (edible? food) (eq? (ask food 'possessor) self))
      (begin
      (ask self 'put 'strength 
           (+ (ask food 'calories) (ask self 'strength)))
      (ask self 'lose food)
      (ask place 'gone food)))))

    
(define-class (restaurant name class price)
    (parent (place name))
    (method (menu) (list (ask class 'name) price))
    (method (sell person order)
        (if (not (eq? order (ask class 'name)))
          (error "this restaurant does not sell" order)
            (if (ask person 'pay-amount price)
              (let ((product (instantiate class)))
                (ask self 'appear product)
                (ask person 'take product))
              #f))))

(define-class (thing name)
    (instance-vars (possessor '()))
    (parent (basic-object name))
    (method (type) 'thing)
    (method (thing?) #t)
    (method (change-possessor new-possessor) (set! possessor new-possessor))
)

(define-class (food name calories)
    (parent (thing name))
    (initialize (begin
                  (ask self 'put 'calories calories)
                  (ask self 'put 'edible? #t))))

(define-class (bagel)
    (class-vars (name 'bagel))
    (parent (food name 150)))

(define-class (basic-object name)
    (instance-vars (table (make-table)))
    (default-method (lookup message table))
    (method (put stat value) (insert! stat value table)))

(define-class (ticket name serial place)
    (parent (thing name))
    (initialize (ask place 'appear self)))

(define-class (laptop name)
    (method (type) 'laptop)
    (parent (thing name))
    (method (connect user-password) (ask (ask self 'place) 
                                         'connect user-password))
    (method (surf website) (ask (ask self 'place) 'surf website)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edible? thing)
  (ask thing 'edible?))

(define-class (thief name initial-place)
  (parent (person name initial-place))
  (instance-vars
   (behavior 'steal))
  (method (type) 'thief)

  (method (notice person)
    (if (eq? behavior 'run)
      (if (null? (ask (usual 'place) 'exis))
        nil
	(ask self 'go (pick-random (ask (usual 'place) 'exits))))
	(let ((food-things
	       (filter (lambda (thing)
			 (and (edible? thing)
			      (not (eq? (ask thing 'possessor) self))))
		       (ask (usual 'place) 'things))))
	  (if (not (null? food-things))
	      (begin
	       (ask self 'take (car food-things))
	       (set! behavior 'run)
	       (ask self 'notice person)) )))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))

(define (announce-unlock name)
  (newline)
  (display name)
  (display " has been unlocked")
  (newline))
(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj)
  (and (procedure? obj)
       (ask obj 'person?)))

(define (thing? obj)
  (and (procedure? obj)
       (ask obj 'thing?)))

