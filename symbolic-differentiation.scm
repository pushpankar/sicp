;; dc/dx = 0
;; dx/dx = 1
;; d(u+v)/dx = du/dx + dv/dx
;; d(uv)/dx = u*(dv/dx) + v*(du/dx)

(define (deriv-exp exp wrt)
  (cond
   ((number? exp) 0)
   ((variable? exp)
    (if (same-variable? exp wrt) 1 0))
   ((sum? exp) (make-sum (deriv-exp (addend exp) wrt)
                         (deriv-exp (augend exp) wrt)))
   ((product? exp) (make-sum (make-product (multiplier exp)
                                           (deriv-exp (multiplicand exp) wrt))
                             (make-product (multiplicand exp)
                                           (deriv-exp (multiplier exp) wrt))))
   ((exponentiation? exp) (make-product (exponent exp)
                                        (make-product (make-exponentiation (base exp)
                                                                           (- (exponent exp) 1))
                                                      (deriv-exp (base exp) wrt))))
   (else (error "Unknown exp: " exp))))

(define (make-sum exp1 exp2)
  (cond
   ((=number? exp1 0) exp2)
   ((=number? exp2 0) exp1)
   ((and (number? exp1) (number? exp2)) (+ exp1 exp2))
   (else (list '+ exp1 exp2))))

(define (make-sum-list l)
  (let ((exprs (filter (lambda (x) (not (= x 0))) l)))
    ('())))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (anyof predicate lats)
  (cond
   ((null? lats) #f)
   (else (or (predicate (car lats)) (anyof predicate (cdr lats))))))

;;
;; (define (make-product exp1 exp2)
;;   (cond
;;    ((or (=zero? exp1) (=zero? exp2)) 0)
;;    ((=number? exp1 1) exp2)
;;    ((=number? exp2 1) exp1)
;;    ((and (number? exp1) (number? exp2)) (* exp1 exp2))
;;    (else (list '* exp1 exp2))))

(define (make-exponentiation base exp)
  (cond
   ((=number? base 1) 1)
   ((=number? exp 0) 1)
   (else (list '** base exp))))

(define (exponentiation? exp)
  (eq? (car exp) '**))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (variable? x)
  (symbol? x))
(define (same-variable? x y)
  (equal? x y))

(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))

(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))

(define (addend exp)
  (cadr exp))

(define (augend exp)
  (caddr exp))

(define (multiplier exp)
  (cadr exp))

(define (multiplicand exp)
  (caddr exp))

(define (=number? x y)
  (if (and (number? x) (number? y))
      (equal? x y)
      false))

(define (=zero? x)
  (=number? x 0))

(deriv-exp 1 'x)
(deriv-exp 'x 'x)
(deriv-exp 'y 'x)
(deriv-exp (make-sum 'x 'x) 'x)

(deriv-exp (make-product (make-product 'x 'x) 'y) 'x)
(deriv-exp (make-exponentiation 'u 5) 'u)


