;; 3.1
(define (make-accumulator init)
  (define (increment value)
    (set! init (+ init value))
    init)
  increment)
(define A (make-accumulator 5))
(A 5)
(A 7)

;; 3.2
(define (make-monitered f)
  (let ((counter 0))
    (define (dispatch message)
      (cond
       ((eq? message 'how-many-calls?) counter)
       ((eq? message 'reset-counter) (begin (set! counter 0) counter))
       (else (begin (set! counter (+ 1 counter)) (f message)))))
    dispatch))

(define ms (make-monitered sqrt))
(ms 5)
(ms 'how-many-calls?)
(ms 'reset-counter)

(define (make-account balance password)
  (define (dispatch pass message)
    (if (eq? pass password)
        (cond
         ((eq? message 'withdraw) (lambda (x) (begin (set! balance (- balance x)) balance)))
         ((eq? message 'deposit)  (lambda (x) (begin (set! balance (+ balance x)) balance))))
        'Incorrect-password))
  dispatch)

(define acc (make-account 100 'secret))
((acc 'secret- 'withdraw) 40)
