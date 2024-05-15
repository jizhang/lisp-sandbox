#lang sicp

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
      (cons (car list1) (append (cdr list1) list2))))

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
