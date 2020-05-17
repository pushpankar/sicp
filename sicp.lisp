;; Chapter 1
;; Excercise 1.3

(defun maxoftwo (a b)
  (cond
    ((> a b) a)
    (t b)))
(maxoftwo 2 3)

(defun maxtwo (a b c)
  (cond
    ((eq (min a b c) a) (cons b c))
    ((eq (min a b c) b) (cons a c))
    (t (cons a b))))

(defun average (x y)
  (/ (+ x y) 2))

(defun sqrt-iter (guess x)
  (if (good-enough guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(defun good-enough (guess x)
  (< (abs (- (* guess guess) x)) 0.0001))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun root (x)
  (sqrt-iter 1.0 x))

(root 2)
;;; Cube root
;;;

(defun cube-root (x)
  (defun cube (x)
    (* x (* x x)))
  (defun good-enough-cube-root (guess x)
    (< (abs (- (cube guess) x)) 0.01))
  (defun improve-cube-root (guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

  (defun cube-root-iter (guess x)

    (if (good-enough-cube-root guess x)
        guess
        (cube-root-iter (improve-cube-root guess x)
                        x)))
  (cube-root-iter 3.0 x))


(cube-root 100000)
(cube 2)
(cube-root-iter 3.0 8)
(good-enough-cube-root 2.0 8)
(improve-cube-root 2.0003 8)
