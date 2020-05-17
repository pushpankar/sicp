(define (square x)
  (* x x))

(square 2)

(define (fib+ x)
  (define (fib-iter n3 n2 n1 counter)
    (cond
     ((<= x 3) x)
     ((= counter x) n1)
     (t (fib-iter n2 n1 (+ n1 n2 n3) (+ 1 counter)))))
  (fib-iter 0 1 2 2))
(fib+ 4)

(define )
