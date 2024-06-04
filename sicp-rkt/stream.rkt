#lang sicp

(define (cons-stream1 a b) (cons a (delay b)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (zero? n)
      (stream-car s)
      (stream-ref (stream-cdr s) (dec n))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (display x) (newline))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (inc low) high))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (cond (already-run?
             (display-line "Use cached result")
             result)
            (else
             (set! result (proc))
             (set! already-run? true)
             result)))))

(define (delay1 exp)
  (memo-proc (lambda () exp)))

(define (force1 delayed-object) (delayed-object))

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
