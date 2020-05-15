(defun atom? (x)
  (not (listp x)))
(+ 1 2)
(atom? 1)

(eq 'hey 'hel)
(eq 1 1)
(cdr ())

(defun lat (x)
  (cond
    ((null x) t)
    ((atom? (car x)) (lat (cdr x)))
    (t nil)))
(cons 'abc '('bbc 'xyz))

(lat '(abc bdc))

(defun mmember (l x)
  (cond
    ((null l) nil)
    (t (or (eq (car l) x)
           (mmember (cdr l) x)))))

(mmember '(abc bdc) 'bdc)

(defun rember (lat a)
  (cond
    ((null lat) lat)
    ((eq (car lat) a) (cdr lat))
    (t (cons (car lat) (rember (cdr lat) a)))))

(rember '(abc bdc l m l) 'l)

(defun firsts (lats)
  (cond
    ((null lats) lats)
    (t (cons (car (car lats)) (firsts (cdr lats))))))
(firsts '((a b c) (e b c)))

(defun insertR (lats new old)
  (cond
    ((null lats) ())
    ((eq (car lats) old) (cons old (cons new (cdr lats))))
    (t (cons (car lats) (insertR (cdr lats) new old)))))

(insertr '(a b c d) 'e 'a)

(defun subst2 (new o1 o2 lats)
  (cond
    ((null lats) ())
    ((or (eq o1 (car lats)) (eq o2 (car lats))) (cons new (cdr lats)))
    (t (cons (car lats) (subst2 new o1 o2 (cdr lats))))))

(subst2 'd 'x 'y '(a b c x y))
