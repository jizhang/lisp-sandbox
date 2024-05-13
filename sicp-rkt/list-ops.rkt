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
