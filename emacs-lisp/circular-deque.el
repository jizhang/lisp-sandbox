(require 'cl-lib)

(cl-defstruct (deque (:constructor deque--create))
  data (head 0) (tail 0))

(defun deque-create (max-size)
  (let ((data (make-vector (1+ max-size) nil)))
    (deque--create :data data)))

(defun deque-size (deque)
  (let ((size (- (deque-tail deque) (deque-head deque))))
    (if (< size 0)
        (+ size (length (deque-data deque)))
      size)))

(defun deque-empty-p (deque)
  (= (deque-head deque) (deque-tail deque)))

(defun deque-full-p (deque)
  (= (deque-head deque) (deque--inc deque (deque-tail deque))))

(defun deque-add-first (deque element)
  (let* ((head (deque-head deque))
         (prev (deque--dec deque head)))
    (when (= prev (deque-tail deque))
      (error "Deque full"))
    (aset (deque-data deque) prev element)
    (setf (deque-head deque) prev)
    t))

(defun deque-add-last (deque element)
  (let* ((tail (deque-tail deque))
         (next (deque--inc deque tail)))
    (when (= next (deque-head deque))
      (error "Deque full"))
    (aset (deque-data deque) tail element)
    (setf (deque-tail deque) next)
    t))

(defun deque-remove-first (deque)
  (let ((head (deque-head deque)))
    (when (= head (deque-tail deque))
      (error "No such element"))
    (let* ((data (deque-data deque))
           (element (aref data head)))
      (aset data head nil)
      (setf (deque-head deque) (deque--inc deque head))
      element)))

(defun deque-remove-last (deque)
  (let ((tail (deque-tail deque)))
    (when (= tail (deque-head deque))
      (error "No such element"))
    (let* ((data (deque-data deque))
           (prev (deque--dec deque tail))
           (element (aref data prev)))
      (aset data prev nil)
      (setf (deque-tail deque) prev)
      element)))

(defun deque-peek-first (deque)
  (aref (deque-data deque) (deque-head deque)))

(defun deque-peek-last (deque)
  (aref (deque-data deque) (deque--dec deque (deque-tail deque))))

(defun deque-as-list (deque)
  (let ((data (deque-data deque))
        (tail (deque-tail deque))
        (i (deque-head deque))
        (result nil))
    (while (/= i tail)
      (push (aref data i) result)
      (setq i (deque--inc deque i)))
    (nreverse result)))

(defun deque--inc (deque i)
  (if (< (cl-incf i) (length (deque-data deque)))
      i
    0))

(defun deque--dec (deque i)
  (if (< (cl-decf i) 0)
      (1- (length (deque-data deque)))
    i))

(let ((deque (deque-create 3)))
  (deque-add-last deque 1)
  (deque-add-last deque 2)
  (princ (deque-remove-first deque) t))

(provide 'circular-deque)
