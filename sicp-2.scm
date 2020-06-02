(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))

(upper-bound (make-interval 2 3))

(define (sub-interval i1 i2)
  (make-interval (- (lower-bound i1) (upper-bound i2))
                 (- (upper-bound i1) (lower-bound i2))))

(sub-interval (make-interval 2 3) (make-interval 3 4))

(define (mul-interval i1 i2)
  (let ((p1 (* (lower-bound i1) (lower-bound i2)))
        (p2 (* (lower-bound i1) (upper-bound i2)))
        (p3 (* (upper-bound i1) (lower-bound i2)))
        (p4 (* (upper-bound i1) (upper-bound i2))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(mul-interval (make-interval 1 2) (make-interval -2 1))

(define (div-interval i1 i2)
  (if (< (* (lower-bound i2)
            (upper-bound i2))
         0)
      '()
      (let ((x (make-interval (/ 1.0 (upper-bound i2))
                              (/ 1.0 (lower-bound i2)))))
        (mul-interval i1 x))))

(div-interval (make-interval -1 2) (make-interval 2 4))
