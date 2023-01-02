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
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree-map x)
             (* x x)))
         tree))
;;2.31
(define (tree-map proc tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map proc x)
             (proc x)))
        tree))
(define (square x) (* x x))
(define (square-tree2 tree) (tree-map square tree))

;;2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)  (cons (car s) x)) rest)))))

;;explanation of 2.32: the important thing to realize is that we need to capture the entire set (1 2 3) before the recursion cdrs down the list.
;;by realizing this, we see that we need to utilize the original argumenet s, and pair the car of s with the recursive call to cdr of s to capture the first entire set.
;;Next iteration then continues the path down the tree until the '() value is nicely added to each subset, completing the list of lists

;2.36
(define (accumulate op init sequence)
  (if (null? sequence)
    init
    (op (car sequence) (accumulate op init (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (w) (matrix-*-vector cols w)) m)))

;2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

;for fold-left and accumulate to provide the same values (op a b) == (op b a)

;2.54
(define (equal-proc? a b)
  (cond ((or (null? a) (null? b)) #t)
        ((or (list? a) (list? b)) (and (equal-proc? (car a) (car b)) (equal-proc? (cdr a) (cdr b))))
        ((eq? a b) #t)
        (else #f)))
    

