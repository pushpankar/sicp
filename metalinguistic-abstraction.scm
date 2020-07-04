;; Utility functions
(define *op-table* (make-hash))
(define (put op proc)
  (hash-set! *op-table* op proc))

(define (get op)
  (hash-ref *op-table* op '()))

(get 'quote)
;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp)
;;          (make-procedure (lambda-parameters exp)
;;                          (lambda-body exp)
;;                          env))
;;         ((begin? exp) (eval-sequence (begin-actions exp) exp))
;;         ((cond? exp) (eval (cond-if exp) env))
;;         ((application? exp) (apply (eval (operator exp) env)
;;                                    (list-of-values (operands exp) env)))
;;         (else (error "Unknown expression type -- EVAL" exp))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else (let ((proc (get (car exp))))
                (if (procedure? proc)
                    (proc exp env)
                    (error "No matching syntax found"))))))

;; Syntax
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


;; self evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(eval "hi" '())

;; variable
(define (variable? exp) (symbol? exp))

;; Quoted expression
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp _env) (cadr exp))
(put 'quote text-of-quotation)

(eval '(quote hello) '())
;; assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(put 'set! eval-assignment)

(eval '(set! 'a 5) '())

;; definition
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (definition? exp)
  (tagged-list? exp 'define))

(put 'define eval-definition)

;; if
(define false 'false)
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(put 'if eval-if)

(eval '(if false 2 1) '())
(variable? '(if false))
(cadr '(if false))

;; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(put 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
                                               (lambda-body exp)
                                               env)))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (eval-sequence exps env)
  (cond ((last-exp? exp) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))
(put 'begin (lambda (exp env) (eval-sequence (begin-actions exp) exp)))

;; cond
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(put 'cond (lambda (exp env) (eval (cond->if exp) env)))

;; application

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(put 'application (lambda (exp env) (apply (eval (operator exp) env)
                                           (list-of-values (operands exp) env))))


;; put functions in the table
(define (let-combination exp)
  (apply (make-procedure (let-vars exp) (let-body exp)) (let-vals exp)))


(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameters procedure)
                                            arguments
                                            (procedure-environment procedure))))
        (else (error "Unknown procedure -- APPLY" procedure))))


;;
;; (define (list-of-values-lr exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (let ((first-value (eval (first-operand exps) env)))
;;         (cons first-value
;;               (list-of-values-lr (rest-operands exps) env)))))

;; (define (eval-if exp env)
;;   (if (true? (eval (if-predicate exp) env))
;;       (eval (if-consequent exp) env)
;;       (eval (if-alternative exp) env)))

;; (define (eval-assignment exp env)
;;   (set-variable-value! (assignment-variable exp)
;;                        (eval (assignment-value exp) env)
;;                        env)
;;   'ok)

;; (define (eval-definition exp env)
;;   (define-variable! (definition-variable exp)
;;     (eval (definition-value exp) env)
;;     env)
;;   'ok)

