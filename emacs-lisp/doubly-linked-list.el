(require 'cl-lib)

(cl-defstruct dnode (value 0) previous next)

(cl-defstruct (dlist (:constructor make-dlist-0))
  head tail)

(defun make-dlist ()
  (let* ((head (make-dnode))
         (tail (make-dnode :previous head)))
    (setf (dnode-next head) tail)
    (make-dlist-0 :head head :tail tail)))

(defun add-to-dlist (dlist element)
  "Append to the end."
  (let* ((tail (dlist-tail dlist))
         (prev (dnode-previous tail))
         (node (make-dnode :value element :previous prev :next tail)))
    (setf (dnode-next prev) node)
    (setf (dnode-previous tail) node)))

(defun remove-from-dlist (dlist)
  "Remove first."
  (let* ((head (dlist-head dlist))
         (node (dnode-next head)))
    (when (eq node (dlist-tail dlist))
      (error "List is empty"))
    (let ((next (dnode-next node)))
      (setf (dnode-next head) next)
      (setf (dnode-previous next) head))
    (dnode-value node)))

(defun clear-dlist (dlist)
  (let ((head (dlist-head dlist))
        (tail (dlist-tail dlist)))
    (setf (dnode-next head) tail)
    (setf (dnode-previous tail) head)))

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
  (princ (dlist-as-list dlist) t))

(provide 'doubly-linked-list)

;; TODO
;; Add and remove by index.
;; Method dispatch. https://nullprogram.com/blog/2018/02/14/
