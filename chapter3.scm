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
  (let ((secure-account (make-monitered (lambda (x) 'password-error))))
    (define (dispatch pass message)
      (if (eq? pass password)
          (begin (secure-account 'reset-counter)
                 (cond
                  ((eq? message 'withdraw) (lambda (x) (begin (set! balance (- balance x)) balance)))
                  ((eq? message 'deposit)  (lambda (x) (begin (set! balance (+ balance x)) balance)))))
          (if (> (secure-account 'how-many-calls?) 3)
              (lambda (x) 'calling-police)
              secure-account)
          ))
    dispatch))

(define acc (make-account 100 'secret))
((acc 'secret 'withdraw) 40)
((acc 'secre 'deposit) 10)

;; Monte carlo

(define (monte-carlo n-trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed n-trials))
          ((experiment) (iter (- trials-remaining 1) (+ 1 trials-passed)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter n-trials 0))

;; 3.5
(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (* (random) range))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral predicate x1 x2 y1 y2 n)
  (define (inside?)
    (let ((rx (random-in-range x1 x2))
          (ry (random-in-range y1 y2)))
      (predicate rx ry)))
  (* (- x2 x1) (- y2 y1) (monte-carlo n inside?)))

(/ (estimate-integral (lambda (x y) (<= (+ (sqr (- x 5)) (sqr (- y 7))) (sqr 3)))
                   2
                   8
                   4
                   10
                   1000000) 9.0)

(estimate-integral (lambda (x y) (<= (+ (* x x) (* y y)) 1.0)) -1.0 1.0 -1.0 1.0 1000000)

(random-in-range 1.0 4.0)
