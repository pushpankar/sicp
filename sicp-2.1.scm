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
