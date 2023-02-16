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



;SICP 3.21 mess around with queues
;the below implementation of queues does not insert the object into the queue twice, the queue object rear pointer
;is simply pointing to the second object as designed. It is keeping track of the end of the list to easily
;insert new items at that location
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x) x)

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue")
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

(define (print-queue queue)
  (display (car queue))
  (display "\n"))

;SICP 3.22 message passing/object implementation of queues with local state
(define (make-queue1)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (insert-queue! item)
      (cond ((null? front-ptr)
             (set! front-ptr item)
             (set! rear-ptr item)
             front-ptr)
            (else
              (set-cdr! rear-ptr item)
              (set! rear-ptr item)
              front-ptr)))
    (define (delete!)
      (cond ((null? front-ptr)
             (error "DELETE! called on an empty queue object"))
            (else
              (set! front-ptr (cdr front-ptr))
              front-ptr)))
    (define (dispatch m . n)
      (cond ((eq? m 'insert-queue!) (insert-queue! n))
            ((eq? m 'delete!) (delete!))
            (else 
              (error "Queue does not recognize procedure call"))))
dispatch))

;SICP 3.23 hmm, I'll have to think about this further. Is it possible to store a copy of the reverse linked list
;in the same list object? getting circular lists with this mehtod. Potenitally create separate copy of reversed list
;locally in a procedure?

(define (make-node init)
  (let ((data init)
        (previous '())
        (next '()))
      (define (set-next! item) (set! next item))
      (define (set-previous! item) (set! previous item))
      (define (set-data! item) (set! data item))
      (define (dispatch m) (cond ((eq? m 'set-next!) set-next!)
                                     ((eq? m 'set-previous!) set-previous!)
                                     ((eq? m 'set-data!) set-data!)
                                     ((eq? m 'data) data)
                                     ((eq? m 'next) next)
                                     ((eq? m 'previous) previous)
                                     (else (error "No defined method" m))))
      dispatch))

(define (make-deque) (cons '() '()))

(define (front-deque queue) (car queue))

(define (rear-deque queue) (cdr queue))

(define (empty-deque? queue)  (null? (front-deque queue)))

(define (set-deque-front-ptr! queue new-item) (set-car! queue new-item))

(define (set-deque-rear-ptr! queue new-item) (set-cdr!  queue new-item))

(define (set-next! item new-next) (((car item) 'set-next!) new-next))

(define (set-previous! item new-previous) (((car item) 'set-previous!) new-previous))

(define (regen-deque queue)
  (if (null? queue)
    '()
    (cons (car queue) (regen-deque ((car queue) 'next)))))

(define (front-insert-deque! queue item)
  (let ((new-item (cons (make-node item) '())))
    (cond ((empty-deque? queue)
           (set-deque-front-ptr! queue new-item)
           (set-deque-rear-ptr! queue new-item)
           queue)
          (else
            (set-cdr! new-item (front-deque queue))
            (set-next! new-item (front-deque queue))
            (set-previous! (front-deque queue) new-item)
            (set-deque-front-ptr! queue new-item)
            queue))))

(define (rear-insert-deque! queue item)
  (let ((new-item (cons (make-node item) '())))
    (cond ((empty-deque? queue)
            (set-deque-front-ptr! queue new-item)
            (set-deque-rear-ptr! queue new-item)
            queue)
           (else 
             (set-cdr! (rear-deque queue) new-item) 
             (set-next! (rear-deque queue) new-item)
             (set-previous! new-item (rear-deque queue))
             (set-deque-rear-ptr! queue new-item)
             queue))))

(define (front-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "Delete! called on empty deque" queue))
        (else 
          (set-deque-front-ptr! queue (cdr (front-deque queue)))
          queue)))

(define (rear-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "Delete! called on empty deque" queue))
        (else
          (set-next! (cons (car ((car (rear-deque queue)) 'previous)) '()) '())
          (set-deque-rear-ptr! queue (car ((car (rear-deque queue)) 'previous)))
          (set-deque-front-ptr! queue (regen-deque (front-deque queue)))
          queue)))
