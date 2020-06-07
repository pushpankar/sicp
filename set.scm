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

(define (intersection_set set1 set2)
  (cond
   ((or (null? set1) (null? set2)) '())
   ((element-of-set? (car set2) set1) (cons (car set2) (intersection_set set1 (cdr set2))))
   (else (intersection_set set1 (cdr set2)))))

(intersection_set (list 1 2 3) (list 2 3 4))
