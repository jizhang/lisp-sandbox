#lang racket
(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))

(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs1 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs2 x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y) (or (> x y) (= x y)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt1 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (factorial1 n)
  (fact-iter 1 1 n))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib1 n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))
