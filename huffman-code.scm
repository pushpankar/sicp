;; constructors and selectors
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))
(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbols object)
  (if (leaf? object)
      (list (symbol-leaf object))
      (caddr object)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))

;; Decoding procedure

(define (decode bits tree)
  (define (decode-1 bits current-branch)
  (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
            (cons (symbols next-branch) (decode-1 (cdr bits) tree))
            (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond
   ((= bit 1) (right-branch branch))
   ((= bit 0) (left-branch branch))
   (else (error "bad bit--- bit can only be 0 or 1"))))

;; Adjoins
(define (adjoin-set x set)
  (cond
   ((null? set) (list x))
   ((< (weight x) (weight (car set))) (cons x set))
   (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (if (leaf? tree)
      (if (eq? (symbol-leaf tree) sym)
          '()
          (error "Invalid Symbol"))
      (let ((left (left-branch tree)))
        (if (memq sym (symbols left))
            (cons 0 (encode-symbol sym left))
            (cons 1 (encode-symbol sym (right-branch tree)))))))

(encode-symbol 'C sample-tree)

(decode (encode '(B A C A B B) sample-tree) sample-tree)


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(make-leaf-set (list sample-tree) (make-leaf 'M 8))

;; doesn't work with single leaf
(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set)))))
;; TESTS
(define leaf-set (make-leaf-set '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))
(successive-merge leaf-set)
(define leaf-set
    (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set)))
(adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set))

(let ((song_tree (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))))
  (encode '(NA) song_tree))

(generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

;; ;;
