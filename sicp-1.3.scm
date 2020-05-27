(define (summations f next start end)
  (cond
   ((> start end) 0)
   (else (+ (f start) (summations f next (next start) end)))))

(define (identity x)
  x)
(summations identity (lambda (x) (+ 1 x)) 1 10)

;;
(define (summations-iterative f next start end)
  (define (iter-sum index total)
    (cond
    ((> index end) total)
    (else (iter-sum (next index) (+ (f index) total)))))
  (iter-sum start 0))

(define (increment x)
  (+ 1 x))
(summations-iterative square increment 1 10)
(increment 1)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (summations f add-dx a b) dx))

(integral cube 0 1 0.0001)

;; 1.31
(define (product term next start end)
  (cond
   ((> start end) 1)
   (else (*
          (term start)
          (product term next (next start) end)))))

(product square increment 1 3)
(define (factorial n)
  (product identity increment 1 n))
(factorial 5)

(define (product-iterative term next start end)
  (define (iter index aggregate)
    (cond
     ((> index end) aggregate)
     (else (iter (next index) (* index aggregate)))))
  (iter start 1))

(product-iterative identity increment 1 5)

(define (accumulate combine null-value term a next b)
  (cond
   ((> a b) null-value)
   (else (combine (term a) (accumulate combine null-value term (next a) next b)))))

(define (add a b)
  (+ a b))

(accumulate (lambda (x y) (+ x y)) 0 square 1 increment 10)

(define (accumulate-iter combine null-value term a next b)
  (define (iter index agg)
    (cond
     ((> index b) agg)
     (else (iter (next index) (combine (term index) agg)))))
  (iter (next (next a)) (combine (term a) (term (next a)))))

(accumulate-iter (lambda (x y) (+ x y)) 0 square 1 increment 10)

(define (accumulate-filter combine null-value term predicate-fun a next b)
  (cond
   ((> a b) null-value)
   ((predicate-fun (term a)) (combine (term a) (accumulate-filter combine null-value term predicate-fun (next a) next b)))
   (else (accumulate-filter combine null-value term predicate-fun (next a) next b))))

(accumulate-filter (lambda (x y) (+ x y)) 0 identity even? 1 increment 10)

;; Fixed point
(define (fixed-point f init)
  (define (iter v)
    (cond
     ((close? v (f v)) v)
     (else (iter (f v)))))
  (define (close? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (iter init))

(fixed-point cos 1)

(define (chi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
(chi)

;;
(define (average x y)
  (/ (+ x y) 2))

(define (avg-damp f)
  (lambda (x)
    (average x (f x))))

(define (root x)
  (fixed-point (avg-damp (lambda (y) (/ x y))) 1.0))

(root 2)

(average 2 3.0)
