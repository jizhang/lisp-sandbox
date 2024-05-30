#lang sicp

(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))

(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs1 x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs3 x)
  (if (< x 0)
      (- x)
      x))

(define (double n) (* n 2))
(define (halve n) (/ n 2))
(define (average x y) (halve (+ x y)))

(define (sqrt x)
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

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt1 b n)
  (define (iter counter product)
    (if (= counter 0)
        product
        (iter (- counter 1) (* b product))))
  (iter n 1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (define (cube n) (* n n n))
  (sum cube a inc b))

(define (sum-integers a b)
  (define (identity n) n)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (term x) (/ 1.0 (* x (+ x 2))))
  (define (next x) (+ x 4))
  (sum term a next b))

(define (sum1 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (pi-sum1 a b)
  (sum1 (lambda (x) (/ 1.0 (* x (+ x 2))))
        a
        (lambda (x) (+ x 4))
        b))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define (search f neg-point pos-point)
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt1 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (let ((dx 0.00001))
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))))

(define (newtons-method g guess)
  (define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))

(define (sqrt2 x)
  (newtons-method
   (lambda (y) (- (square y) x )) 1.0))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (cons1 x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operation: CONS" m))))
  dispatch)

(define (car1 z) (z 'car))
(define (cdr1 z) (z 'cdr))

(define (cons2 x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation: CONS" m))))
  dispatch)

(define (car2 z) (z 'car))
(define (cdr2 z) (z 'cdr))
(define (set-car2! z new-value)
  ((z 'set-car!) new-value))
(define (set-cdr2! z new-value)
  ((z 'set-cdr!) new-value))
