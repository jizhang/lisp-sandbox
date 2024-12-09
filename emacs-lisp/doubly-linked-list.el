(require 'cl-lib)

(cl-defstruct dnode (value 0) previous next)

(cl-defstruct (dlist (:constructor make-dlist-0))
  head tail)

(defun make-dlist () ; TODO From list
  (let* ((head (make-dnode))
         (tail (make-dnode :previous head)))
    (setf (dnode-next head) tail)
    (make-dlist-0 :head head :tail tail)))

;;; Add
(defun insert-dnode (dlist element previous next)
  (let ((node (make-dnode :value element :previous previous :next next)))
    (setf (dnode-next previous) node)
    (setf (dnode-previous next) node)
    node))

(defun add-to-dlist (dlist element)
  "Append to the end."
  (let* ((tail (dlist-tail dlist))
         (prev (dnode-previous tail)))
    (insert-dnode dlist element prev tail))
  t)

(defun add-to-dlist-at (dlist element index)
  (let* ((prev (dlist-head dlist))
         (next (dnode-next prev))
         (tail (dlist-tail dlist)))
    (while (and (> index 0) (not (eq next tail)))
      (setq prev next)
      (setq next (dnode-next next))
      (cl-decf index))
    (unless (zerop index) (error "Index out of bounds"))
    (insert-dnode dlist element prev next))
  t)

(defun add-first-to-dlist (dlist element)
  (add-to-dlist-at dlist element 0))

;; TODO Add all

;;; Remove
(defun remove-dnode (dnode)
  (let ((prev (dnode-previous dnode))
        (next (dnode-next dnode)))
    (setf (dnode-next prev) next)
    (setf (dnode-previous next) prev))
  (dnode-value dnode))

(defun remove-from-dlist-at (dlist index)
  (let* ((head (dlist-head dlist))
         (node (dnode-next head))
         (tail (dlist-tail dlist)))
    (while (and (> index 0) (not (eq node tail)))
      (setq node (dnode-next node))
      (cl-decf index))
    (unless (and (zerop index) (not (eq node tail)))
      (error "No such element"))
    (remove-dnode node)))

(defun remove-from-dlist (dlist)
  "Remove first."
  (remove-from-dlist-at dlist 0))

(defun remove-last-from-dlist (dlist)
  (let* ((tail (dlist-tail dlist))
         (node (dnode-previous tail)))
    (when (eq node (dlist-head dlist))
      (error "No such element"))
    (remove-dnode node)))

(defun clear-dlist (dlist)
  (let ((head (dlist-head dlist))
        (tail (dlist-tail dlist)))
    (setf (dnode-next head) tail)
    (setf (dnode-previous tail) head))
  t)

;;; Search
(defun dlist-index-of (dlist element)
  (let* ((head (dlist-head dlist))
         (node (dnode-next head))
         (tail (dlist-tail dlist))
         (index 0))
    (while (and (not (eq node tail)) (not (equal (dnode-value node) element)))
      (setq node (dnode-next node))
      (cl-incf index))
    (if (eq node tail)
        -1
      index)))

(defun dlist-contains (dlist element)
  (>= (dlist-index-of dlist element) 0))

;;; Misc
(defun dlist-size (dlist)
  (let* ((head (dlist-head dlist))
         (node (dnode-next head))
         (tail (dlist-tail dlist))
         (size 0))
    (while (not (eq node tail))
      (cl-incf size)
      (setq node (dnode-next node)))
    size))

(defun dlist-as-list (dlist)
  (let* ((head (dlist-head dlist))
         (tail (dlist-tail dlist))
         (node (dnode-previous tail))
         (result nil))
    (while (not (eq node head))
      (push (dnode-value node) result)
      (setq node (dnode-previous node)))
    result))

(let ((dlist (make-dlist)))
  (add-to-dlist dlist 1)
  (add-to-dlist dlist 2)
  (remove-from-dlist dlist)
  (add-to-dlist dlist 3)
  (add-to-dlist-at dlist 4 1)
  (remove-from-dlist-at dlist 2)
  (add-first-to-dlist dlist 3)
  (remove-last-from-dlist dlist)
  (princ (dlist-as-list dlist) t))

(provide 'doubly-linked-list)
