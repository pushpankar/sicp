;; It took me so much time to realize recursion can be
;; used bottom up as well
(defun fib (x)
  (defun fib-next (prev curr counter)
    (cond
      ((eq counter x) curr)
      (t (fib-next curr (+ curr prev) (1+ counter)))))
  (cond
    ((< x 2) x)
    (t (fib-next 1 1 2))))

(fib 10000)

