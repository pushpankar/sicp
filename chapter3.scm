;; 3.1
(define (make-accumulator init)
  (define (increment value)
    (set! init (+ init value))
    init)
  increment)
(define A (make-accumulator 5))
(A 5)
(A 7)

;; 3.2
(define (make-monitered f)
  (let ((counter 0))
    (define (dispatch message)
      (cond
       ((eq? message 'how-many-calls?) counter)
       ((eq? message 'reset-counter) (begin (set! counter 0) counter))
       (else (begin (set! counter (+ 1 counter)) (f message)))))
    dispatch))

(define ms (make-monitered sqrt))
(ms 5)
(ms 'how-many-calls?)
(ms 'reset-counter)

(define (make-account balance password)
  (define (make-authenticator pass)
    (lambda (secret) (eq? secret pass)))

  (define (make-error-notifier)
    (let ((mf (make-monitered (lambda (x) 'wrong-password))))
      (lambda (x)
        (if (> (mf 'how-many-calls?) 2)
            'calling-police
            (mf 'pass-err)))))

  (define (transactor message)
    (cond
     ((eq? message 'withdraw) (lambda (x) (begin (set! balance (- balance x)) balance)))
     ((eq? message 'deposit)  (lambda (x) (begin (set! balance (+ balance x)) balance)))))

  (define (make-secure-account authenticator transactor)
    (let ((notify-error (make-error-notifier)))
      (lambda (pass message)
        (if (authenticator pass)
            (if (eq? message 'joint)
                (lambda (x) (make-secure-account (make-authenticator x) transactor))
                (transactor message))
            notify-error))))
  (make-secure-account (make-authenticator password) transactor)
  )

(define (make-joint-acc acc pass new-pass)
  ((acc pass 'joint) new-pass))

(define acc (make-account 100 'hello))
((acc 'hello 'withdraw) 10)
((acc 'hello 'deposit) 10)
(define mf (lambda (x) 'wp))
(mf 'how-many-calls?)

(define joint-acc (make-joint-acc acc 'hello 'joint-pass))
((joint-acc 'joint-pass 'withdraw) 10)

(define (make-joint partner-acc partner-pass password)
  (if ((partner-acc 'verify) partner-pass)
      (partner-acc 'dispatcher)
      'Wrong-password))

(define acc (make-account 100 'secret))
((acc 'secret 'withdraw) 40)
((acc 'secret 'deposit) 10)

;; Monte carlo

(define (monte-carlo n-trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed n-trials))
          ((experiment) (iter (- trials-remaining 1) (+ 1 trials-passed)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter n-trials 0))

;; 3.5
(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (* (random) range))))

(define (estimate-integral predicate x1 x2 y1 y2 n)
  (define (inside?)
    (let ((rx (random-in-range x1 x2))
          (ry (random-in-range y1 y2)))
      (predicate rx ry)))
  (* (- x2 x1) (- y2 y1) (monte-carlo n inside?)))

(/ (estimate-integral (lambda (x y) (<= (+ (sqr (- x 5)) (sqr (- y 7))) (sqr 3)))
                   2
                   8
                   4
                   10
                   1000000) 9.0)

(estimate-integral (lambda (x y) (<= (+ (* x x) (* y y)) 1.0)) -1.0 1.0 -1.0 1.0 1000000)

(random-in-range 1.0 4.0)


;; 3.22

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define (make-queue)
  (let ((front-pointer '())
        (rear-pointer '()))
    (let ((queue (cons front-pointer rear-pointer)))
      (define (insert x)
        (let ((new-pair (cons x '())))
          (if (null? front-pointer)
              (begin (set! front-pointer new-pair) (set! rear-pointer new-pair))
              (begin (set-cdr! rear-pointer new-pair) (set! rear-pointer new-pair)))))
      (define (delete-queue!)
        (if (null? front-pointer)
            '()
            (begin (set! front-pointer (cdr front-pointer)) (delete-queue!))))

      (define (print-queue)
        front-pointer)
      (define (dispatch message)
        (cond
         ((eq? message 'insert) (lambda (x) (insert x)))
         ((eq? message 'print) (print-queue))
         (else (error "no procedure found"))))
      dispatch)))

(define q (make-queue))
(q 'insert)
((q 'insert) 7)
(q 'print)




;;

(define (assoc key records same-key?)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records) same-key?))))

;; local tables
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup . keys)
      (define (lookup-helper subtable key . keys)
        (let ((record (assoc key (cdr subtable) same-key?)))
          (if record
              (if (null? keys)
                  (cdr record)
                  (apply lookup-helper (append (list record) keys)))
              false)))
      (apply lookup-helper (append (list local-table) keys)))

    (define (insert! value . keys)
      (define (insert-helper! subtable key . keys)
        (let ((record (assoc key (cdr subtable) same-key?)))
          (if record
              (if (null? keys)
                  (set-cdr! record value)
                  (apply insert-helper! (append (list record) keys)))
              (set-cdr! subtable (cons (cons key1 (apply ((make-table same-key?) 'insert-proc!) (append keys (list value))))
                                       (cdr subtable)))
              )))
      (apply insert-helper! (append (list local-table) keys))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'x1 'y2 'v2)
(get 'x1 'y2)
