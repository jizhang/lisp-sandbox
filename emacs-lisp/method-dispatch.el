;; -*- lexical-binding: t -*-
(require 'ert)

(defun md-cons (x y)
  (let* ((setx (lambda (v) (setq x v)))
         (sety (lambda (v) (setq y v)))
         (dispatch (lambda (m)
                     (cond ((eq m 'car) x)
	                   ((eq m 'cdr) y)
                           ((eq m 'setcar) setx)
                           ((eq m 'setcdr) sety)
	                   (t (error "Undefined operation: CONS %s" m))))))
    dispatch))

(defun md-car (z) (funcall z 'car))
(defun md-cdr (z) (funcall z 'cdr))
(defun md-setcar (z v) (funcall (funcall z 'setcar) v))
(defun md-setcdr (z v) (funcall (funcall z 'setcdr) v))

(let ((c (md-cons 1 2)))
  (md-setcdr c 3)
  (princ (md-cdr c) t))

(ert-deftest md-test-cons ()
  (let ((c (md-cons 1 2)))
    (should (equal (md-car c) 1))
    (should (equal (md-cdr c) 2))

    (md-setcar c 3)
    (should (equal (md-car c) 3))
    (should (equal (md-cdr c) 2))

    (md-setcdr c 4)
    (should (equal (md-cdr c) 4))
    (should (equal (md-car c) 3))

    (md-setcdr c 5)
    (should (equal (md-cdr c) 5))

    (should-error (funcall c 'undef))))
