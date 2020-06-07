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

(define (add-interval i1 i2)
  (make-interval (+ (lower-bound i1) (lower-bound i2))
                 (+ (upper-bound i1) (upper-bound i2))))

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

(define (make-center-percent c p)
  (let ((diff (* c p)))
    (make-interval (- c diff) (+ c diff))))

(define (center-p i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))
(define (tolerace-p i)
  (/ (- (upper-bound i) (center-p i)) (center-p i)))

(define v (make-center-percent 2 0.5))
(center-p v)
(tolerace-p v)

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(par1 (make-interval 1 1.01) (make-interval 3 3.01))
(par2 (make-interval 1 1.01) (make-interval 3 3.01))

(let ((A (make-center-percent 300 0.01))
      (B (make-center-percent 1 0.01)))
  (add-interval A B))


(define (f x y . z)
  (+ z)

(f 1 2 3 4)

(define (same-parity h . t)
  (if (even? h)
      (cons h (filter even? t))
      (cons h (filter odd? t))))

(same-parity 2 3 4 5)


;; Nested mapping

(define (enumerate-interval start end)
  (if (> start end)
      '()
      (cons start (enumerate-interval (+ 1 start)
                                      end))))
(define (accumulate combine dummy sequence)
  (cond
   ((null? sequence) dummy)
   (else (combine (car sequence) (accumulate combine dummy (cdr sequence))))))

(define (flat-map proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pair n)
  (flat-map
   (lambda (x) (map (lambda (y) (list y x))
                    (enumerate-interval 0 x)))
   (enumerate-interval 0 n)))

(unique-pair 5)

(define (unique-triples n)
  (flat-map (lambda (x)
         (map (lambda (y) (cons x y))
              (unique-pair x )))
       (enumerate-interval 0 n)))

(define (sum l)
  (cond
   ((null? l) 0)
   (else (+ (car l) (sum (cdr l))))))

(define (triples-of-sum s n)
  (filter (lambda (x) (= (sum x) s))
          (unique-triples n)))

(lambda (x)
  (enumerate-interval 0 x))

(triples-of-sum 10 5)
(unique-triples 3)

(sum '(1 2 3))

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

;; flipped-pairs is abstraction of wave4
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vertical painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (corner-split painter (- n 1)))
            (row (below painter smaller)))
        (beside row (flip-horizontal row)))))

(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split2 painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split2 painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below (beside smaller smaller) painter))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  ((square-of-four identity flip-vert identity flip-vert) painter))

(define (square-limit painter)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split2 painter n))))

(define (split op1 op2)
  (lambda (painter)
    (op1 painter (op2 painter painter))))
(define right-split (split beside below))
(define up-split (split below beside))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))



;; Section 2.3
(list 'a 'b)
(list (list 'geor))
(cadr '((x1 x2) (y1 y2)))
(cdr '((x1 x2) (y1 y2)))
