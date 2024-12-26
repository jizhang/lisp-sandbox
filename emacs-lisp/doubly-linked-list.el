;; -*- lexical-binding: t -*-
(require 'generator)
(require 'cl-lib)

(cl-defstruct dnode (value 0) previous next)

(cl-defstruct (dlist (:constructor make-dlist-0))
  head tail)

(defun make-dlist (&optional elements)
  (let* ((head (make-dnode))
         (tail (make-dnode :previous head)))
    (setf (dnode-next head) tail)
    (let ((dlist (make-dlist-0 :head head :tail tail)))
      (dlist-add-all dlist elements)
      dlist)))

;;; Add
(defun dnode--insert (dlist element previous next)
  (let ((node (make-dnode :value element :previous previous :next next)))
    (setf (dnode-next previous) node)
    (setf (dnode-previous next) node)
    node))

(defun dlist-add (dlist element)
  "Append to the end."
  (let* ((tail (dlist-tail dlist))
         (prev (dnode-previous tail)))
    (dnode--insert dlist element prev tail))
  t)

(defun dlist-add-at (dlist element index)
  (let* ((prev (dlist-head dlist))
         (next (dnode-next prev))
         (tail (dlist-tail dlist)))
    (while (and (> index 0) (not (eq next tail)))
      (setq prev next)
      (setq next (dnode-next next))
      (cl-decf index))
    (unless (zerop index) (error "Index out of bounds"))
    (dnode--insert dlist element prev next))
  t)

(defun dlist-add-first (dlist element)
  (dlist-add-at dlist element 0))

(defun dlist-add-all (dlist elements)
  (let* ((tail (dlist-tail dlist))
         (prev (dnode-previous tail)))
    (dolist (element elements)
      (let ((node (make-dnode :value element :previous prev)))
        (setf (dnode-next prev) node)
        (setq prev node)))
    (setf (dnode-next prev) tail)
    (setf (dnode-previous tail) prev))
  t)

;;; Remove
(defun dnode--remove (dnode)
  (let ((prev (dnode-previous dnode))
        (next (dnode-next dnode)))
    (setf (dnode-next prev) next)
    (setf (dnode-previous next) prev))
  (dnode-value dnode))

(defun dlist-remove-at (dlist index)
  (let* ((head (dlist-head dlist))
         (node (dnode-next head))
         (tail (dlist-tail dlist)))
    (while (and (> index 0) (not (eq node tail)))
      (setq node (dnode-next node))
      (cl-decf index))
    (unless (and (zerop index) (not (eq node tail)))
      (error "No such element"))
    (dnode--remove node)))

(defun dlist-remove (dlist)
  "Remove first."
  (dlist-remove-at dlist 0))

(defun dlist-remove-last (dlist)
  (let* ((tail (dlist-tail dlist))
         (node (dnode-previous tail)))
    (when (eq node (dlist-head dlist))
      (error "No such element"))
    (dnode--remove node)))

(defun dlist-clear (dlist)
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

(iter-defun dlist-iterator (dlist &optional reversed)
  (let* ((head (dlist-head dlist))
         (tail (dlist-tail dlist))
         (from (if reversed tail head))
         (to (if reversed head tail))
         (next-fn (if reversed #'dnode-previous #'dnode-next))
         (node (funcall next-fn from)))
    (while (not (eq node to))
      (iter-yield (dnode-value node))
      (setq node (funcall next-fn node)))))

(defun dlist-as-list (dlist &optional reversed)
  (cl-loop
   for value
   iter-by (dlist-iterator dlist reversed)
   collect value))

(let ((dlist (make-dlist)))
  (dlist-add dlist 1)
  (dlist-add dlist 2)
  (dlist-remove dlist)
  (dlist-add dlist 3)
  (dlist-add-at dlist 4 1)
  (dlist-remove-at dlist 2)
  (dlist-add-first dlist 3)
  (dlist-remove-last dlist)
  (princ (dlist-as-list dlist) t))

(provide 'doubly-linked-list)
