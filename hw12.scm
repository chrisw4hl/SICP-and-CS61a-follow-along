;SICP 4.1.1-4.1.6: Metacircular Evaluator
;
;4.3, starting with m-eval code from SICP
(define (meval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (meval (cond->if exp) env))
        ((application? exp)
         (apply1 (meval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: M-EVAL" exp))))

(define (apply1 procedure arguments) ;rewriting the definition of actual apply was messing with built in functions such as (display)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type: Apply" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (meval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (meval (if-predicate exp) env))
    (meval (if-consequent exp) env)
    (meval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (meval (first-exp exps) env))
        (else
          (meval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (meval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (meval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clauses isn't last: COND->IF"
                 clauses))
        (make-if (cond-predicate first)
                 (sequency->exp (cond-actions first))
                 (expand-clauses rest))))))

(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
;Acutal start of exercise 4.3: rewrite m-eval in data directed style:

(define (meval1 exp env)
  (cond ((self-evaluating? exp) exp)
        ((get 'proc (car exp)) ((get 'proc (car exp)) (cdr exp) env))
        ((application? exp) (apply (meval1 (car exp) env)
                                   (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: EVAL" exp))))

;need to install special forms for get to find, and refactor the special forms so that they expect only the argument exp.

;4.6 SICP; I think this should work 
(define (let->combination exp)
  (cons (make-lambda (map car (cadr exp)) (caddr exp)) (map cadr (cadr exp)))) 

;4.7 SICP
;nested lets are expanded to become lambda statements that are called with their repsective parameters
;the internally nested procedure bodies are called with the extenrnal let* parameters
;I believe the procedure (let*->nested-lets should be possible as a derived expression, as long as we
;maintain the convention of let* that parameters are defined sequentially from left to right.

;4.10 SICP
;new syntax can be defined by changing the data-directed procedures to be installed with the eval-procedure.
;Need to implement and think of new syntax example for this one.

;4.11 SICP: rewrite the frame representation to represent a frame as a list of name-value pairs

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (map (lambda (x y) (cons x y)) variables values))

(define (frame-variables frame) (map car frame))

(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
    (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
    (env-loop env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame)) (cdar frame))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
      (error "Unbound-variable" var)
      (let ((frame (first-frame env)))
        (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame)) (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
        (scan frame))))
  (env-loop env))
    
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame) (frame-values frame))))

(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals frame1)
      (cond ((null? vars)
             '())
            ((eq? var (car vars)) (delete vars frame1))
            (else (scan (cdr vars) (cdr vals) (cdr frame1)))))
    (scan (frame-variables frame) (frame-values frame) frame)))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '- -)
        (list 'integer? integer?)
        (list 'list-ref list-ref)
        (list 'not not)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")

(define output-prompt ";;; M-Eval value:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (meval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define apply-in-underlying-scheme apply)

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))


(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
(define initial-env (extend-environment (primitive-procedure-names)
                                        (primitive-procedure-objects)
                                        the-empty-environment))

(define (make-unbound! var env) ;version of code for implementation of environments using a variable list and a value list
   (let* ((frame (first-frame env)) 
          (vars (car frame)) 
          (vals (cadr frame))) 
     (define (scan pre-vars pre-vals vars vals) 
       (if (not (null? vars)) 
           (if (eq? var (car vars)) 
               (begin (set-cdr! pre-vars (cdr vars)) 
                      (set-cdr! pre-vals (cdr vals))) 
               (scan vars vals (cdr vars) (cdr vals))))) 
     (if (not (null? vars)) 
         (if (eq? var (car vars)) 
             (begin (set-car! frame (cdr vars)) 
                    (set-cdr! frame (cons (cdr vals) '())) )
             (scan vars vals (cdr vars) (cdr vals))))))

;;4.13 make-unbound with above help from schemewiki.org to understand how to manipulate list structure
;;implemented with environment changes from 4.11 to change representation of environments as list of name-value pairs
(define (make-unbound-pairs! var env)
  (let* ((frame (first-frame env)))
    (define (scan pre-frame frame)
      (if (not (null? frame))
        (if (eq? var (caar frame))
          (set-cdr! pre-frame (cdr frame)))
        (scan frame (cdr frame))))
    (if (not (null? frame))
      (if (eq? var (caar frame))
        (set-car! frame (cdr frame))
        (scan frame (cdr frame))))))

;;4.14
;I believe the built in map function requires a more specialized version of apply, which applies the map procedure
;to any arbitrary number of lists. This built in function does not play nicely with the apply procedure
;we are using for this Metacircular evaluator.

;;4.15
;if the halts? procedure exists as described, then evaluating (try try) will never terminate if (halts? p a) works
;as intended. If halts ever provides a value for a terminated arguments p,a then (try try) will never terminate.
;This is against the intended behavior of halts?

;CS61A HW12 part 2: Implement type checking of arguments to procedures


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (begin 
      (define (extend-environment-iter vars vals base-env hold)
        (cond ((null? vars) (cons hold base-env))
              ((pair? (car vars))
               (if (not (meval (car vars) (extend-environment (list (cadar vars)) (list (car vals)) base-env)))
                 (error "Wrong argument type -- " (car vals))
                 (extend-environment-iter (cdr vars) (cdr vals) base-env (cons (cons (cadar vars) (car vals)) hold))))
              (else 
                (extend-environment-iter (cdr vars) (cdr vals) base-env (cons (cons (car vars) (car vals)) hold)))))

    (extend-environment-iter vars vals base-env '()))
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

