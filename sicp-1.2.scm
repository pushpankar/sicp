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
