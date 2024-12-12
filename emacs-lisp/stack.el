(require 'cl-lib)

;;; List stack
(cl-defstruct stack
  (head nil)
  (size 0))

(defun stack-push (s element)
  (setf (stack-head s) (cons element (stack-head s)))
  (cl-incf (stack-size s))
  nil)

(defun stack-pop (s)
  (when (zerop (stack-size s)) (error "No such element"))
  (let ((element (car (stack-head s))))
    (setf (stack-head s) (cdr (stack-head s)))
    (cl-decf (stack-size s))
    element))

(defun stack-peek (s)
  (car (stack-head s)))

;;; Array stack
(cl-defstruct (array-stack (:constructor make-array-stack-0))
  elements
  (size 0))

(defun make-array-stack (max-size)
  (let ((elements (make-vector max-size 0)))
    (make-array-stack-0 :elements elements)))

(defun array-stack-push (s element)
  (when (= (length (array-stack-elements s)) (array-stack-size s))
    (error "Stack is full"))
  (aset (array-stack-elements s) (array-stack-size s) element)
  (cl-incf (array-stack-size s))
  nil)

(defun array-stack-pop (s)
  (when (zerop (array-stack-size s)) (error "No such element"))
  (aref (array-stack-elements s) (cl-decf (array-stack-size s))))

(defun array-stack-peek (s)
  (let ((size (array-stack-size s)))
    (if (zerop size)
        nil
      (aref (array-stack-elements s) (1- size)))))

(provide 'stack)
