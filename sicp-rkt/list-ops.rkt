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
