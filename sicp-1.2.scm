;; 1.11
(define (fib+ x)
  (define (fib-iter n3 n2 n1 counter)
    (cond
     ((<= x 3) x)
     ((= counter x) n1)
     (else (fib-iter n2 n1 (+ n1 n2 n3) (+ 1 counter)))))
  (fib-iter 0 1 2 2))
(fib+ 6)

;; 1.12
(define (moving-sum xs)
  (cond
   ((< (length xs) 2) ())
   ((cons (+ (car xs) (car (cdr xs))) (moving-sum (cdr xs))))))

(define (row xs)
  (cons (cons 1 (moving-sum xs)) 1))

(row '(1 3 3 1))
;;1.17
;;
;;
(define (fast-exp x n)
  (cond
   ((= n 0) 1)
   ((even? n)
    (square (fast-exp x (/ n 2))))
   (else (* x (fast-exp x (- n 1))))))
(fast-exp 4 28)
;; x^n = (x^2)^(n/2) even
;; x^n = x* x^(n-1) = x * (x^2)^((n-1)/2) odd
(define (fast-exp-iter x n a)
  (cond
   ((= n 0) a)
   ((even? n) (fast-exp-iter (square x) (/ n 2) a))
   (else (fast-exp-iter x (- n 1) (* a x)))))

(fast-exp-iter 4 28 1)

;; 1.17
;; a * b = (a + a + ... ntimes) = (a * b/2) + (a * b/2)
;; a * b = a * (b-1) + a
(define (fast-mul a b)
  (cond
   ((= b 1) a)
   ((even? b) (double (fast-mul a (/ b 2))))
   (else (+ a (fast-mul a (- b 1) )))))

(define (double x)
  (+ x x))
(fast-mul 3 8)

;; gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 101011019283782 573)
