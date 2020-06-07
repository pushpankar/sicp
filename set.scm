(define (element-of-set? x set)
  (cond
   ((null? set) false)
   ((equal? (car set) x) true)
   (else (element-of-set? x (cdr set)))))

(element-of-set? 'x (list 'z 1 2 'x))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(adjoin-set 'z (list 'y 'z))

(define (intersection-set set1 set2)
  (cond
   ((or (null? set1) (null? set2)) '())
   ((element-of-set? (car set2) set1) (cons (car set2) (intersection_set set1 (cdr set2))))
   (else (intersection_set set1 (cdr set2)))))

(intersection-set (list 1 2 3) (list 2 3 4))

(define (union-set set1 set2)
  (cond
   ((null? set1) set2)
   ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
   (else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set (list 1 2 3) (list 2 3 4 5))

;; 2.60:: Duplicates
(define element-of-dup-set? element-of-set?)
(define (adjoin-dup-set x xs)
  (cons x xs))

(define (intersection-dup-set xs ys)
  (cond
   ((or (null? xs) (null? ys)) '())
   ((element-of-dup-set? (car xs) ys) (cons (car xs) (intersection-dup-set (cdr xs) ys)))
   (else (intersection-dup-set (cdr xs) ys))))

(intersection-dup-set (list 1 2 2 3) (list 2 3 3 4))

(define (union-dup-set xs ys)
  (append xs ys))

;; ordered set
(define (element-of-set? x set)
  (cond
   ((null? set) '())
   ((= x (car set)) true)
   ((> x (car set)) (element-of-set? x (cdr set)))
   (else false)))

(element-of-set? 6 (list 1 5 6 7))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond
         ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
         ((> x1 x2) (intersection-set set1 (cdr set2)))
         (else (intersection-set (cdr set1) set2))))))

(intersection-set (list 1 2 3 4) (list 2 3 5))

(define (adjoin-set x set)
  (cond
   ((null? set) (list x))
   ((= x (car set)) set)
   ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
   (else (cons x set))))

(adjoin-set 2 (list 1 4 5))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond
         ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
         ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
         ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))))))

(union-set (list 1 2 3 4) (list 3 4 6 7 9))

;; set as tree

(define (entry tree)
  (cadr tree))
(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list left entry right))
(define (element-of-set? x set)
  (cond
   ((null? set) false)
   ((= x (entry set)) true)
   ((> x (entry set)) (element-of-set? x (right-branch set)))
   ((< x (entry set)) (element-of-set? x (left-branch set)))))

(define (adjoin-set x set)
  (cond
   ((null? set) (list '() x '()))
   ((= x (entry set)) set)
   ((> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))
   ((< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))))

(define (tree-list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

;; Excercise 2.64
;; Procedure list-> tree takes an ordered list. Construct a left tree of half size. Next
;; element will be center and the rest element will go to the right.
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- (- n 1) left-size)))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

(left-branch (left-branch (list->tree (list 0 1 2 3 4 5 6 7 8 9 10 11 12))))

(define (union-set set1 set2)
  (define (merge l1 l2)
    (if (null? l1)
        l2
        (let ((x1 (car l1))
              (x2 (car l2)))
          (cond
           ((= x1 x2) (cons x1 (merge (cdr l1) (cdr l2))))
           ((< x1 x2) (cons x1 (merge (cdr l1) l2)))
           ((> x1 x2) (cons x2 (merge l1 (cdr l2))))))))
  (let ((sl1 (tree-list-2 set1))
        (sl2 (tree-list-2 set2)))
    (list->tree (merge sl1 sl2))))

(union-set (list->tree (list 0 1 2 3 8 9 10 11 12))  (list->tree (list 0 1 2 3 4 5 6 7 8 9 10 11 12)))

(define (intersection-set set1 set2)
  (define (intersection-ordered-list l1 l2)
    (if (or (null? l1) (null? l2))
        '()
        (let ((x1 (car l1))
              (x2 (car l2)))
          (cond
           ((= x1 x2) (cons x1 (intersection-ordered-list (cdr l1) (cdr l2))))
           ((> x1 x2) (intersection-ordered-list l1 (cdr l2)))
           (else (intersection-ordered-list (cdr l1) l2))))))
  (let ((sl1 (tree-list-2 set1))
        (sl2 (tree-list-2 set2)))
    (list->tree (intersection-ordered-list sl1 sl2))))


(intersection-set (list->tree (list 0 1 2 3 8 9 10 11 12))  (list->tree (list 0 1 2 3 4 5 6 7 8 9 12)))
