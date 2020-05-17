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

(defun add1 (x)
  (+ x 1))
(add1 -2)
(+ 1 2 4 5)
(cons 1 2)

(defun tup+ (tup1 tup2)
  (cond
    ((or (null tup1) (null tup2)) ())
    (t (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))

(tup+ '(3 7) '(4 6))

(defun latlen (lats)
  (cond
    ((null lats) 0)
    (t (+ 1 (latlen (cdr lats))))))
(latlen '(1 2 2 4 3))

(defun occur (lats n)
  (cond
    ((null lats) 0)
    ((eq n (car lats)) (+ 1 (occur (cdr lats) n)))
    (t (occur (cdr lats) n))))

(occur '(1 2 1 3 1) '1)

;; Chapter 5
(defun remberplus (lats a)
  (cond
    ((null lats) lats)
    ((listp (car lats)) (cons (remberplus (car lats) a) (remberplus (cdr lats) a)))
    ((eq (car lats) a) (remberplus (cdr lats) a))
    (t (cons (car lats) (remberplus (cdr lats) a)))))

(remberplus '((a b c) ((((c) c b))) c) 'c)

(defun occursomething (a l)
  (cond
    ((null l) 0)
    ((listp (car l)) (+ (occursomething a (car l)) (occursomething a (cdr l))))
    ((eq (car l) a) (+ 1 (occursomething a (cdr l))))
    (t (occursomething a (cdr l)))))

(occursomething 'a '((a b ((a b a))) a))

(defun numbers? (l)
  (cond
    ((null l) t)
    ((numberp (car l)) (numbers? (cdr l)))
    (t nil)))

(defun numberorexp (l)
  (cond
    ((null l) t)
    ((or (numberp (car l)) (isaexp (car l))) (numberorexp (cdr l)))
    (t nil)))

(defun isaexp (aexp)
  (cond
    ((atom? aexp) (numberp aexp))
    (t
      (cond
        ((null aexp) t)
        ((eq (car aexp) '+) (numberorexp (cdr aexp)))
        ((eq (car aexp) '*) (numberorexp (cdr aexp)))
        (t nil)))))

(numberp '*)
(isaexp '(* 1 (+ 2 1) 2))

(firsts '((1 2) (3 4)))

(defun set? (lats)
  (cond
    ((null lats) t)
    (t (cond
          ((mmember (cdr lats) (car lats)) nil)
          (t (set? (cdr lats)))))))

(set? '(1 2 3 1 3))

(defun dict? (lats)
  (set? (firsts lats)))

(dict? '((1 2) (2 3)))

(defun revdict (lats)
  (cond
    ((null lats) ())
    (t (cons (cons (car (cdr (car lats))) (car (car lats))) (revdict (cdr lats))))))


(revdict '((1 2) (2 3)))
