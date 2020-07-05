;; Utility functions
(define *op-table* (make-hash))
(define (put op proc)
  (hash-set! *op-table* op proc))

(define (get op)
  (hash-ref *op-table* op '()))

;; (define (meval exp env)
;;   (cond ((self-mevaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (meval-assignment exp env))
;;         ((definition? exp) (meval-definition exp env))
;;         ((if? exp) (meval-if exp env))
;;         ((lambda? exp)
;;          (make-procedure (lambda-parameters exp)
;;                          (lambda-body exp)
;;                          env))
;;         ((begin? exp) (meval-sequence (begin-actions exp) exp))
;;         ((cond? exp) (meval (cond-if exp) env))
;;         ((application? exp) (apply (meval (operator exp) env)
;;                                    (list-of-values (operands exp) env)))
;;         (else (error "Unknown expression type -- MEVAL" exp))))

(define (meval exp env)
  (cond ((self-mevaluating? exp) exp)
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


;; self mevaluating
(define (self-mevaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(meval "hi" '())

;; variable
(define (variable? exp) (symbol? exp))

;; Quoted expression
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp _env) (cadr exp))
(put 'quote text-of-quotation)

(meval '(quote hello) '())

;; assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (meval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (meval (assignment-value exp) env)
                       env)
  'ok)

(put 'set! meval-assignment)

;; definition
(define (meval-definition exp env)
  (define-variable! (definition-variable exp)
                    (meval (definition-value exp) env)
                    env)
  'ok)

(define (definition? exp)
  (tagged-list? exp 'define))

(put 'define meval-definition)

;; if
(define false 'false)
(define true 'true)
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (meval-if exp env)
  (if (true? (meval (if-predicate exp) env))
      (meval (if-consequent exp) env)
      (meval (if-alternative exp) env)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(put 'if meval-if)
(meval '(if false 1 0) '())
;; (print (get (car '(if false 1 0))))

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

(define (meval-sequence exps env)
  (cond ((last-exp? exp) (meval (first-exp exps) env))
        (else (meval (first-exp exps) env)
              (meval-sequence (rest-exps exps) env))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))
(put 'begin (lambda (exp env) (meval-sequence (begin-actions exp) exp)))

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

(put 'cond (lambda (exp env) (meval (cond->if exp) env)))

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
      (cons (meval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(put 'application (lambda (exp env) (apply (meval (operator exp) env)
                                           (list-of-values (operands exp) env))))


;; put functions in the table
(define (let-combination exp)
  (apply (make-procedure (let-vars exp) (let-body exp)) (let-vals exp)))


(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (meval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameters procedure)
                                            arguments
                                            (procedure-environment procedure))))
        (else (error "Unknown procedure -- APPLY" procedure))))


;;
;; (define (list-of-values-lr exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (let ((first-value (meval (first-operand exps) env)))
;;         (cons first-value
;;               (list-of-values-lr (rest-operands exps) env)))))

;; (define (meval-if exp env)
;;   (if (true? (meval (if-predicate exp) env))
;;       (meval (if-consequent exp) env)
;;       (meval (if-alternative exp) env)))

;; (define (meval-assignment exp env)
;;   (set-variable-value! (assignment-variable exp)
;;                        (meval (assignment-value exp) env)
;;                        env)
;;   'ok)

;; (define (meval-definition exp env)
;;   (define-variable! (definition-variable exp)
;;     (meval (definition-value exp) env)
;;     env)
;;   'ok)

(define (and vals)
  (cond ((null? vals) true)
        ((meval (car vals)) (and (cdr vals)))
        (else false)))
;; While

(define (while? exp) (eq? (car exp) 'while))
(define (while-predicate exp) (cadr exp))
(define (while-actions exp) (cddr exp))
(define (make-while exp) (cons 'while exp))
(define (meval-while exp env) (if (eval (while-predicate exp) exp)
                                 (meval (make-begin (append (while-actions exp) (make-while exp))) exp)
                                 'ok))


;; Section 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

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
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
