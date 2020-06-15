;; (require racket/trace)
;; Utility functions
(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define *coersion-table* (make-hash))
(define (put-coersion from to proc)
  (hash-set! *coersion-table* (list from to) proc))
(define (get-coersion from to)
  (hash-ref *coersion-table* (list from to) '()))


;; try next on fail
(define (all arg)
  (cond
   ((null? arg) true)
   ((car arg) (all (cdr arg)))
   (else false)))

(define (some arg)
  (cond
   ((null? arg) false)
   ((car arg) true)
   (else (some (cdr arg)))))

(define (square x)
  (mul x x))

(define (max-of comparator args)
  (cond
   ((null? args) '())
   ((= (length args) 1) (car args))
   (else (let ((max-rest (max-of comparator (cdr args)))
               (a (car args)))
           (if (comparator a max-rest)
               a
               max-rest)))))


(define (try-seq op args-list)
  (if (null? args-list)
      '()
      (let ((res (op (car args-list))))
        (if (null? res)
            (try-seq op (cdr args-list))
            res))))

(define (all-or-none fn args)
  (let ((res (map fn args)))
    (let ((success? (map (lambda (x) (not (null? x))) res)))
      (if (all success?)
          res
          '()))))

;; helper for generic operations
(define (attach-tag tag x)
  (cons tag x))

(define (get-tag x)
  (if (pair? x)
      (car x)
      'scheme-number))

(define (contents x)
  (if (pair? x)
      (cdr x)
      x))


(define (coerse source-args)
  (define (successive-raise arg to)
    (if (equal? (get-tag arg) to)
        arg
        (successive-raise (mraise arg) to)))

  (let ((target-type (get-tag (max-of (compose not subset-of?) source-args))))
      (all-or-none (lambda (arg) (successive-raise arg target-type)) source-args)))


(define (apply-generic op . args)
  (let ((tags (map get-tag args)))
    (let ((proc (get op tags)))
      (if (procedure? proc)
          (apply proc (map contents args))
          (let ((coersed-result (coerse args)))
            (if (null? coersed-result)
                (error "No valid mapping")
                (apply apply-generic (append (list op) coersed-result))))))))

;; Generic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (eq? x y) (apply-generic 'eq? x y))
(define (cosine x) (apply-generic 'cosine x))
(define (sine x) (apply-generic 'sine x))
(define (sqrt-generic x ) (apply-generic 'sqrt-generic x ))
(define (subset-of? a b) (let ((t1 (get-tag a))
                               (t2 (get-tag b)))
                           (cond
                            ((equal? t1 t2) false)
                            ((equal? t1 'complex) false)
                            (else (let ((raised (mraise a)))
                                    (if (equal? (get-tag raised) t2)
                                        true
                                        (subset-of? raised b)))))))

(define (mraise x) (if (equal? (get-tag x) 'complex)
                      x
                      (apply-generic 'mraise x)))

(define (project x) (if (equal? (get-tag x) 'scheme-number)
                      x
                      (apply-generic 'project x)))

(define (drop x) (let ((projector (get 'project (list (get-tag x)))))
                   (if (procedure? projector)
                       (if (eq? (mraise (project x)) x)
                           (project x)
                           x)
                       x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Number package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-scheme-number)
  ;; internal packages
  (define (tag x) x)
  (define (make x) (tag x))
  (define (add x y) (tag (+ x y)))
  (define (sub x y) (tag (- x y)))
  (define (mul x y) (tag (* x y)))
  (define (div x y) (tag (/ x y)))
  (define (eq? x y) (= x y))
  (define (sqrt-num x) (tag (sqrt x)))
  (define (cosine-num x) (cos x))
  (define (sine-num x) (sin x))
  (define (subset-of? a b) (let ((t1 (get-tag a))
                                 (t2 (get-tag b)))
                             (and (equal? t1 'scheme-number) (equal? t2 'rational))))

  ;; interface
  (put 'add '(scheme-number scheme-number) add)
  (put 'sub '(scheme-number scheme-number) sub)
  (put 'mul '(scheme-number scheme-number) mul)
  (put 'div '(scheme-number scheme-number) div)
  (put 'eq? '(scheme-number scheme-number) eq?)
  (put 'cosine '(scheme-number) (lambda (x) (tag (cosine-num x))))
  (put 'sine '(scheme-number) (lambda (x) (tag (sine-num x))))
  (put 'subset-of? '(scheme-number) subset-of?)
  (put 'make 'scheme-number make)
  (put 'sqrt-generic '(scheme-number) sqrt-num)
  (put 'mraise '(scheme-number) (lambda (x) (make-rational x 1)))

  'done)


;; constructor for the system
(install-scheme-number)


(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

;; test scheme num package
;; (drop (div (make-scheme-number 5) (make-scheme-number 6)))
(eq? (make-scheme-number 5) (make-scheme-number 5))
(sqrt-generic (make-scheme-number 4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Rational package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (eq? x y)
    (and (apply-generic 'eq? (numer x) (numer y))
         (apply-generic 'eq? (denom x) (denom y))))
  (define (sqrt-rational x) (make-rat (sqrt-generic (numer x))
                                      (sqrt-generic (denom x))))

  (define (cosine-rat x) (tag (cos (/ (numer x)
                                  (denom x)))))
  (define (sine-rat x) (tag (sin (/ (numer x)
                                (denom x)))))

  (define (subset-of? a b) (let ((t1 (get-tag a))
                                 (t2 (get-tag b)))
                             (and (equal? t1 'rational) (equal? t2 'complex))))
  (define (project x) (make-scheme-number (numer x)))


  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'sqrt-generic '(rational) (lambda (x) (tag (sqrt-rational x))))


  (put 'cosine '(rational) cosine-rat)
  (put 'sine '(rational) sine-rat)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'eq? '(rational rational) eq?)
  (put 'subset-of? '(rational) subset-of?)
  (put 'project '(rational) project)
  (put 'mraise '(rational) (lambda (x) (make-complex-from-real-imag (tag x) 0)))
  'done)

;; constructors for the rest of the system
(define (make-rational n d)
  ((get 'make 'rational) n d))

;; test
(install-rational-package)
(eq? (make-rational 4 5) (make-rational 8 10))
(mul (make-rational 4 5) (make-rational 4 5))
(div (make-rational 4 5) (make-rational 4 5))
(square (make-rational 5 4))
(drop (sqrt-generic (make-rational 16 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Rectangular Complex package ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt-generic (add (square (real-part z))
                       (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cos a)) (mul r (sin a))))


  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Polar Complex package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (add (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Complex package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (subset-of? a b) false)

  (define (eq-complex? z1 z2)
    (and (eq? (real-part z1) (real-part z2))
         (eq? (imag-part z1) (imag-part z2))))

  (define (project z)
    (real-part z))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'eq? '(complex complex) eq-complex?)
  (put 'project '(complex) project)
  'done)

;; contructors
(install-complex-package)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Tests
(eq? (make-complex-from-real-imag 5 6) (make-complex-from-real-imag 5 6))
(apply-generic 'magnitude (make-complex-from-mag-ang 5 6))

(coerse (list (make-complex-from-real-imag 5 3) (make-scheme-number 5)))
(add (make-complex-from-real-imag 5 3) (make-scheme-number 5))
(drop  (add (make-complex-from-real-imag 5 3) (make-scheme-number 5)))

(sub (mraise (mraise (make-scheme-number 6))) (make-scheme-number 4))

(subset-of? (make-complex-from-real-imag 5 8) (make-complex-from-real-imag 6 7))

(coerse (list (make-scheme-number 5) (make-complex-from-real-imag 5 6) (make-rational 5 6)))
;; (make-rational 4 2)

;; (mraise (mraise (make-scheme-number 5)))
(add (make-complex-from-real-imag 5 6) (make-complex-from-real-imag 5 6))
(apply-generic 'project (make-complex-from-real-imag 5 6))
(drop (make-complex-from-real-imag (make-rational 5 3) 2))
;; (project (make-rational 5 1))
;; (mul (make-complex-from-mag-ang 9 2) (make-rational 4 5))
(apply-generic 'magnitude (mraise (make-rational 3 4)))
(let ((z (mraise (make-rational 3 4))))
    (add (square (apply-generic 'real-part z))
                       (square (apply-generic 'imag-part z))))
;; (add (square (apply-generic 'real-part (mraise (make-rational 3 4))))
(apply-generic 'real-part (mraise (make-rational 3 4)))
;; (sqrt-generic (make-rational 4 9))
(apply-generic 'magnitude (make-complex-from-mag-ang 3 4))
(apply-generic 'magnitude (make-complex-from-real-imag (make-rational 1 4) 0))
(mul (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4))
(div (mraise (make-rational 4 1)) (make-complex-from-real-imag 3 4))
(mul (make-complex-from-mag-ang 4 1) (make-rational 3 4))
(apply-generic 'angle (make-complex-from-mag-ang 4 (make-rational 2 2)))
(drop (make-rational 2 1))
