;; Generic ops

(define (variable? x)
  (symbol? x))

(define (same-variable? x y)
  (equal? x y))


(define (deriv exp var)
  (cond
   ((number? exp) 0)
   ((variable? exp) (if (same-variable? exp var) 1 0))
   (else ((get 'deriv (operator exp)) (operands exp) var))))


(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (install-sum-package)
  ;; internal procedures
  (define (make-sum args)
    (let ((total (apply + (filter number? args)))
          (vars (filter (compose not number?) args)))
      (cond
       ((null? vars)  total)
       ((= total 0) (if (= (length vars) 1)
                        vars
                        (tag vars)))
       (else (tag (append vars (list total)))))))

  (define (deriv-sum exps var) (make-sum (map (lambda (x) (deriv x var))) exps))

  ;; interface to the rest of the system
  (define (tag args) (cons '+ args))
  (put 'make-exp '+  make-sum)
  (put 'deriv '+ deriv-sum)
  'done)

;; Assuming here that there will only be one type tag
(define (install-product-package)
  (define (multiplier exp)
    (cadr exp))
  (define (multiplicand exp)
    (cddr exp))
  (define (make-sum . exps)
    ((get 'make-exp '+) exps))
  (define (make-product . args)
    (let ((total (apply * (filter number? args)))
          (vars (filter (compose not number?) args)))
      (cond
       ((null? vars) total)
       ((= total 0) 0)
       ((= total 1) (if (= (length vars) 1)
                        vars
                        (tag vars)))
       (else (tag (append vars (list total)))))))

  (define (deriv-product exp var) (make-sum (make-product (multiplier exp)
                                                          (deriv (multiplicand exp) wrt))
                                            (make-product (multiplicand exp)
                                                          (deriv (multiplier exp) wrt))))

  ;; interface
  (define (tag args) (cons '* args))
  (put 'make-exp '* make-product)
  (put 'deriv '* deriv-product)
  'done
  )

;; 2.74
((get 'get-record 'division1) empid)
