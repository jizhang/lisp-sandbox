#lang racket

(define (square n) (* n n))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (smallest-divisor n)
  (define (divides? a b)
    (zero? (remainder b a)))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (inc test-divisor)))))
  (find-divisor 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((zero? exp) 1)
        ((even? exp)
         (remainder
          (square (expmod base (halve exp) m))
          m))
        (else
         (remainder
          (* base (expmod base (dec exp) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (inc (random (dec n)))))

(define (fast-prime? n times)
  (cond ((zero? times) true)
        ((fermat-test n) (fast-prime? n (dec times)))
        (else false)))