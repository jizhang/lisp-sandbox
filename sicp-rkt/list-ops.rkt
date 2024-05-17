#lang sicp

(define (square x) (* x x))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (list-ref1 items n)
  (if (zero? n)
      (car items)
      (list-ref (cdr items) (dec n))))

(define (length1 items)
  (if (null? items)
      0
      (inc (length1 (cdr items)))))

(define (length2 items)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (inc count))))
  (iter items 0))

(define (append1 list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append1 (cdr list1) list2))))

(define (last-pair items)
  (if (or (null? items) (null? (cdr items)))
      items
      (last-pair (cdr items))))

(define (reverse items)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (cons (car rest) result) (cdr rest))))
  (iter nil items))

(define (same-parity a . w)
  (define valid? (if (even? a) even? odd?))
  (define (iter result rest)
    (cond ((null? rest) result)
          ((valid? (car rest))
           (iter (append result (list (car rest)))
                 (cdr rest)))
          (else
           (iter result (cdr rest)))))
  (iter (list a) w))

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map1 proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map1 proc (cdr items)))))

(define (scale-list1 items factor)
  (map (lambda (x) (* x factor)) items))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(define (for-each1 proc items)
  (cond ((not (null? items))
         (proc (car items))
         (for-each1 proc (cdr items)))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (fringe items)
  (if (null? items)
      nil
      (let ((first (car items))
            (rest (cdr items)))
        (if (pair? first)
            (append (fringe first) (fringe rest))
            (cons first (fringe rest))))))

(define (fringe1 items)
  (cond ((null? items) nil)
        ((not (pair? items)) (list items))
        (else (append (fringe1 (car items))
                      (fringe1 (cdr items))))))

(define (fringe2 items)
  (define (iter items result)
    (cond ((null? items) result)
          ((pair? items)
           (iter (car items)
                 (iter (cdr items) result)))
          (else (cons items result))))
  (iter items nil))

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (inc low) high))))

(define enumerate-tree fringe1)

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))

; Exercise 2.33
(define (map2 p sequence)
  (accumulate
   (lambda (x y) (cons (p x) y))
   nil
   sequence))

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length3 sequence)
  (accumulate
   (lambda (x y) (inc y))
   0
   sequence))

; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Exercise 2.38
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; Exercise 2.39
(define (reverse1 sequence)
  (fold-right
   (lambda (x y) (append y (list x)))
   nil
   sequence))

(define (reverse2 sequence)
  (fold-left
   (lambda (x y) (cons y x))
   nil
   sequence))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
