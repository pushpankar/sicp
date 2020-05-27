;; Excercise 2.1
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (eq-rat x y)
  (= (* (numer x) (denom y))
     (* (numer x) (denom y))))

(define (gcd x y)
  (cond
   ((eq? (modulo x y) 0) y)
   (else (gcd y (modulo x y)))))

(gcd 9 90)
(negative? 5)

(define (make-rat n d)
  (cond
   ((negative? d) (make-rat (* -1 n) (* -1 d)))
   (else (cons (/ n (gcd n d)) (/ d (gcd n d))))))

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(add-rat (make-rat 2 -1) (make-rat 1 2))

;; Excercise 2.2

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint s)
  (make-point (/ (+ (x-point (start-segment s))
                    (x-point (end-segment s)))
                 2)
              (/ (+ (y-point (start-segment s))
                    (y-point (end-segment s)))
                 2)))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(midpoint (make-segment (make-point 0 0) (make-point 4 4)))
;; Excercise 2.3
;; points here should be diagonals
(define (make-rect p1 p2)
  (cons p1 p2))

(define (rect-area r)
  (let ((p1 (car r))
        (p2 (cdr r)))
    (* (- (x-point p1) (x-point p2))
       (- (y-point p1) (y-point p2)))))

(define (perim r)
  (let ((p1 (car r))
        (p2 (cdr r)))
    (* 2 (+ (- (x-point p1) (x-point p2))
            (- (y-point p1) (y-point p2))))))

(rect-area (make-rect (make-point 1 0) (make-point 2 3)))
(perim (make-rect (make-point 1 0) (make-point 2 3)))


;; Excercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
;; proof

(car (cons 1 2))
(car (lambda (m) (m 1 2)))
((lambda (m) (m 1 2)) (lambda (p q) p))
((lambda (p q) p ) 1 2)
(1)

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))
;; ((lambda (f) (lambda (x) (f ((n f) x))))
;;  (lambda (f) (lambda (x) x)))
;; (lambda (x) (lambda (f) (lambda (x) x)) ((n)))
;; 2.17
(define (last-pair l)
  (cond
   ((null? l) l)
   ((null? (cdr l)) (car l))
   (else (last-pair (cdr l)))))
(last-pair (list 1 2 3 7))

;; Excercise 2.17
(define (reverse-list l)
  (cond
   ((null? l) l)
   (else (cons (reverse-list (cdr l)) (car l)))))

(reverse-list (list 1 2 3 4))

;; 2.20
(filter even? (list 1 2 3 4))

(define (same-parity x . xs)
  (cond
   ((even? x) (filter even? (cons x xs)))
   (else (filter odd? (cons x xs)))))

(same-parity (list 1 2 3 4))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    (answer)))))
  (iter items nil))
(square-list (list 1))

(define (deep-reverse x)
  (cond
   ((null? x) x)
   ((pair? x) (cons (deep-reverse (cdr x))
                    (deep-reverse (car x))))
   (else x)))
(define x
  (list (list 1 2) (list 3 4)))
(reverse x)
(deep-reverse x)

;; Exercise 2.29

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
;; I didn't understand the question well. What it is trying to ask.

;; 2.30 and 2.31
(define (square x)
  (* x x))
(define (square-tree tree)
  (cond
   ((null? tree) '())
   ((pair? tree) (cons (square-tree (car tree))
                       (square-tree (cdr tree))))
   (else (square tree))))

(square-tree (list 1 (list 2 4) 3))

(define (map-tree f tree)
  (cond
   ((null? tree) '())
   ((pair? tree) (cons (map-tree f (car tree))
                       (map-tree f (cdr tree))))
   (else (f tree))))
(map-tree square (list 1 (list 2 4) 3))

;;
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append (map (lambda (x)
                       (cons (car s) x))
                     rest)
                rest))))

(subsets (list 1 2 3 5))

;; Excercise 2.33
(define (accumulate combine null-value term a next b)
  (cond
   ((> a b) null-value)
   (else (combine (term a) (accumulate combine null-value term (next a) next b)))))

(define (accumulate combine dummy sequence)
  (cond
   ((null? sequence) dummy)
   (else (combine (car sequence) (accumulate combine dummy (cdr sequence))))))


(define (map p sequence)
  (accumulate (lambda (x y) (p x y))
             nil
             sequence))
(accumulate * 1 (list 1 2 3 7))

(define (m-append seq1 seq2)
  (cond
   ((null? seq1) seq2)
   (else (cons (car seq1)
               (m-append (cdr seq1) seq2)))))
(m-append (list 1 2 3) (list 6 7 8))
(define (append-acc seq1 seq2)
  (accumulate cons seq2 seq1)
  )
(append-acc (list 1 2 3) (list 6 7 8))

(define (m-length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))
(m-length (list 1 2 3 4 8 0))

;; Example 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

;; Excercise 2.35
(define (count-leaves t)
  (accumulate +
              1
              (map count-leaves t)))
(count-leaves (list 1 2))

(define (accumulate-n op init seqs)
  (cond
   ((null? seqs))
   (else (cons (accumulate op init (map car seqs))
               (accumulate-n op init (map cdr seqs))))))

(define (accumulate-ns op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-ns op init (map cdr seqs)))))
(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map f (cdr l)))))

(accumulate-ns + 0 (list (list 1 2 3) (list 1 2 3) (list 1 2 3)))

(define (matrix-*-vectort m v)
  (map (lambda (x)
         (accumulate-ns * 1 (list x v)))
       m))

(matrix-*-vectort (list (list 1 2 3)
                        (list 4 5 6)
                        (list 3 2 4))
                  (list 1 2 3))

(accumulate-ns * 1 (list (list 1 2 3) (list 1 2 3)))

(define (transpose mat)
  (accumulate-ns cons '() mat))
(transpose (list (list 1 2 3) (list 1 2 3)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vectort cols x))
         m)))

(matrix-*-matrix (list (list 1 2) (list 1 2)) (list (list 1 2) (list 1 2)))
(matrix-*-vectort '((1 1) (2 2)) '(1 2))

;; Excercise 2.39
(define (reverse1 sequence)
  (foldr (lambda (x y) (append y (list x)))
         '()
         sequence))

(define (reverse2 sequence)
  (foldl (lambda (x y) (cons x y))
         '()
         sequence))

(reverse1 (list 1 2 3 4))
(reverse2 (list 1 2 3 4))

(define (enumerate-interval start end)
  (if (> start end)
      '()
      (cons start (enumerate-interval (+ 1 start)
                                      end))))

(define (all-pairs start end)
  (accumulate append
              '()
              (map (lambda (i) (map (lambda (j) (list i j))
                                    (enumerate-interval start i)))
                   (enumerate-interval start end))))

(all-pairs 1 5)

(define (flat-map proc seq)
  (accumulate append '() (map proc seq)))

(define (all-pairs2 start end)
  (flat-map (lambda (i)
              (map (lambda (j)
                     (list i j))
                   (enumerate-interval start i)))
            (enumerate-interval start end)))
(all-pairs2 1 5)
