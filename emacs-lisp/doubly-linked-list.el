(defun make-dnode (value &optional previous next)
  (record 'dlist-node value previous next))

(defun set-previous-dnode (dnode previous) (aset dnode 2 previous))
(defun set-next-dnode (dnode next) (aset dnode 3 next))
(defun dnode-value (dnode) (aref dnode 1))
(defun previous-dnode (dnode) (aref dnode 2))
(defun next-dnode (dnode) (aref dnode 3))

(defun make-dlist ()
  (let* ((head (make-dnode 0))
         (tail (make-dnode 0 head)))
    (set-next-dnode head tail)
    (record 'dlist head tail)))

(defun dlist-head (dlist) (aref dlist 1))
(defun dlist-tail (dlist) (aref dlist 2))

(defun add-to-dlist (dlist element)
  "Append to the end."
  (let* ((tail (dlist-tail dlist))
         (prev (previous-dnode tail))
         (node (make-dnode element prev tail)))
    (set-next-dnode prev node)
    (set-previous-dnode tail node)))

(defun remove-from-dlist (dlist)
  "Remove first."
  (let* ((head (dlist-head dlist))
         (node (next-dnode head)))
    (when (eq node (dlist-tail dlist))
      (error "List is empty"))
    (let ((next (next-dnode node)))
      (set-next-dnode head next)
      (set-previous-dnode next head))
    (dnode-value node)))

(defun clear-dlist (dlist)
  (let ((head (dlist-head dlist))
        (tail (dlist-tail dlist)))
    (set-next-dnode head tail)
    (set-previous-dnode tail head)))

(defun dlist-as-list (dlist)
  (let* ((head (dlist-head dlist))
         (tail (dlist-tail dlist))
         (node (previous-dnode tail))
         (result nil))
    (while (not (eq node head))
      (push (dnode-value node) result)
      (setq node (previous-dnode node)))
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
;; Method dispatch.
;; Use `cl-defstruct' or `defclass'.
