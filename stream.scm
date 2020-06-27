(define (add-stream s1 s2)
  (stream-map + s1 s2))
(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))
(define ints (cons-stream 1 (add-stream ones ints)))

(stream-ref ints 11)

(define factorials (cons-stream 1 (mul-stream (stream-cdr ints)
                                              factorials)))

(define fibs (cons-stream 0 (cons-stream 1 (add-stream (stream-cdr fibs)
                                                       fibs))))
(stream-ref fibs 12)

(stream-ref factorials 5)

(define (partial-sum S)
  (cons-stream (stream-car S)
               (add-stream (partial-sum S)
                           (stream-cdr S))))
(stream-ref (partial-sum ints) 3)


;;  3.59
;; Power series
(define (div-series s1 s2)
  (cons-stream (/ (stream-car s1) (stream-car s2))
               (div-series (stream-cdr s1) (stream-cdr s2))))

(define (integrate-series s)
  (div-series s ints))

(stream-map + ints ints ints ints)

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(stream-head (sqrt-stream 2) 5)
(stream-ref (sqrt-stream 2) 2)


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(stream-head (euler-transform (sqrt-stream 2)) 5)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define (scale-stream s val)
  (stream-map (lambda (x) (* x val)) s))

(define pi-stream (scale-stream (partial-sum pi-summands) 4))
(define pi-stream (stream-map (lambda (x) (* x 4))
                              pi-summands))
