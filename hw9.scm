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


;SICP 3.25
;below make-table code modified from SICP to allow arbitrary number of keys for 3.25
(define (make-table)
  (let ((local-table (list '*table*)))

    (define (record? subtable)
      (if (and (not (pair? (cdr subtable))) subtable)
        #t
        #f))

    (define (lookup key-list table)
      (let ((subtable
              (assoc (car key-list) (cdr table))))
        (cond ((and subtable (record? subtable) (null? (cdr key-list)))
                subtable) ;(cons table subtable) to return both the previous level and current value pairs
              ((and subtable (null? (cdr key-list)) (not (record? subtable))) subtable)
              ((not subtable) #f)
              (else (lookup (cdr key-list) subtable)))))

    (define (insert! key-list value table)
      (let ((subtable
              (assoc (car key-list) table)))
        (if subtable
            (if (record? subtable)
                subtable
                (insert! (cdr key-list) value subtable))
            (set-cdr! table
                      (cons (gen-struct key-list value)
                      (cdr table)))))
      'ok)

    (define (gen-struct key-list val)
      (if (null? (cdr key-list))
        (cons (car key-list) val)
        (list (car key-list) (gen-struct (cdr key-list) val))))

    (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc!) insert!)
          ((eq? m 'table) local-table)
          (else (error "Unknown operation: TABLE" m))))

  dispatch))

(define (gen-struct key-list val)
      (if (null? (cdr key-list))
        (cons (car key-list) val)
        (list (car key-list) (gen-struct (cdr key-list) val))))

;SICP 3.27
;(memo-fib f) as described in SICP 3.3.3 computes the nth fibonacci number in a number of steps proportional to n.
;The procedure accomplishes this by checking if the result has been previously computed during the recursive computation.
;If the value is already in the table, the function just pulls the result from the table. The special form (or)
;allows scheme to only compute the body of the let in the or if there is no value (fib x) already stored in the table.
;If the memoize function must actually compute the value, it saves the value in the table, then returns the result.
;this allows the coupled functions to only calculate the value of (fib x) once, even if conventionally the
;value would be calculated many times through recursion
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      false)))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value)
                (cdr table)))))
  'ok)

(define (make-table1)
  (list '*table*))

(define (memoize f)
  (let ((table (make-table1)))
    (lambda (x)
      (let ((previously-computed-result
              (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize
    (lambda (n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (memo-fib (- n 1))
                     (memo-fib (- n 2))))))))

;HW9 #1 vector-append
(define (vector-append a b)
  (let* ((counta (vector-length a))
        (countb (vector-length b))
        (len (+ counta countb)))
    (define (loop newvec n)
      (if (< n 0)
        newvec
        (if (>= n counta)
          (begin (vector-set! newvec n (vector-ref b (- n counta)))
                 (loop newvec (- n 1)))
          (begin (vector-set! newvec n (vector-ref a n))
                 (loop newvec (- n 1))))))
    (loop (make-vector len) (- len 1))))

;HW9 #2 vector-filter. This version is faster than converting the input vector to list, filtering as list,
;then returning vector by converting list to vector. It is faster because the conversions all require
;computations of order n, then the filter itself requires order n instructions. By runnning through the vector
;then holding the passed items in a list, we are saving computations. Then converting the held list to a vector
;requires order n computations. For each item in the vector, checking the pred then adding the item to hold
;are constant time operations.
(define (vector-filter pred v)
  (let ((len (vector-length v)))
    (define (loop n hold)
      (if (< n 0)
        (list->vector hold) 
        (if (pred (vector-ref v n))
          (loop (- n 1) (cons (vector-ref v n) hold))
          (loop (- n 1) hold))))
    (loop (- len 1) '()))) 

;HW9 #3 bubble-sort; prove in notebook
(define (bubble-sort v)
  (let ((len (vector-length v))
        (hold 0))
    (define (loop n count)
      (cond ((and (= n (- count 1)) (= count 1)) v)
            ((= n (- count 1)) (loop 0 (- count 1)))
            ((> (vector-ref v n) (vector-ref v (+ n 1)))
                (begin (set! hold (vector-ref v n)) 
                        (vector-set! v n (vector-ref v (+ n 1))) 
                 (vector-set! v (+ n 1) hold) (loop (+ n 1) count)))
            (else (loop (+ n 1) count))))
    (loop 0 len)))
