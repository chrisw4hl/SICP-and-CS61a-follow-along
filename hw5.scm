;CS61A HW5
;2.24
;skipped, I think I understand
;
;2.25
(define (pick-226a list)
  (car (cdaddr list)))

(define (pick-226b list)
  (caar list))

(define (pick-226c list)
  (cadadr list))
;2.26
;;(append x y) would return (list 1 2 3 4 5 6)
;;(cons x y) would return a list, with a list of (1 2 3) as the first element (list (list 1 2 3) 4 5 6)
;;The result of (cons x y) is not a list with two sublists, because the second argument of cons becomes a sublist
;;(list x y) returns a list of two sublists
;;
;;2.29
;(define (make-mobile left right) (list left right))

;(define (make-branch length structure) (list length structure))

;(define (branch-length branch) (car branch))

;(define (left-branch mobile) (car mobile))

;(define (right-branch mobile) (car (cdr mobile)))

;(define (branch-structure branch) (car (cdr branch)))

(define (branch? mobile) (if (number? (car mobile))
                             #t
                             #f))

(define (total-weight mobile)
  (cond((and (branch? mobile) (number? (branch-structure mobile))) (branch-structure mobile))
       ((branch? mobile) (total-weight (branch-structure mobile)))
       (else (+ (total-weight (left-branch mobile)) (total-weight (right-branch mobile))))))

(define (balance mobile)
  (if (equal? (torque (left-branch mobile)) (torque (right-branch mobile)))
      #t
      #f))

(define (torque branch)
  (* (car branch) (total-weight branch)))

;;;2.29 d) how much of the mobile representation would we have to change if the constructors are modified to:
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))
;;;
;;;These changes to the constructors require appropriate changes to the selectors to ensure the correct data is still selected and passed to the functions utilizing the selectors
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile)) ;this selector is different. There is no need to remove the empty list
(define (branch-structure branch) (cdr branch)) ;also no need to remove the empty list
(define (branch-length) (car branch)) ;also no need to remove empty list

;2.30
;
