(require 'cl-lib)

(cl-defstruct (bst-node (:constructor bst-node-create))
  value left right)

(cl-defun bst-node-replace
    (node &key (left (bst-node-left node)) (right (bst-node-right node)))
  (bst-node-create :value (bst-node-value node) :left left :right right))

(cl-defstruct (bst (:constructor bst-create))
  root)

(cl-defun bst-insert (bst value)
  (let ((node (bst-root bst))
        (new-node (bst-node-create :value value)))
    (when (null node)
      (setf (bst-root bst) new-node)
      (cl-return-from bst-insert))

    (while (bst-node-p node)
      (cond ((< value (bst-node-value node))
             (when (null (bst-node-left node))
               (setf (bst-node-left node) new-node)
               (cl-return-from bst-insert))
             (setf node (bst-node-left node)))

            ((null (bst-node-right node))
             (setf (bst-node-right node) new-node)
             (cl-return-from bst-insert))

            (t
             (setf node (bst-node-right node)))))))

(defun bst-insert-immutable (bst value)
  (cl-labels
      ((insert (node value)
         (cond ((null node) (bst-node-create :value value))
               ((< value (bst-node-value node))
                (bst-node-replace
                 node :left (insert (bst-node-left node) value)))
               (t
                (bst-node-replace
                 node :right (insert (bst-node-right node) value))))))
    (setf (bst-root bst) (insert (bst-root bst) value))))

(let ((bst (bst-create)))
  (bst-insert-immutable bst 2)
  (bst-insert-immutable bst 3)
  (bst-insert-immutable bst 1)
  (bst-insert-immutable bst 4)
  (princ (bst-root bst)t ))

(provide 'binary-search-tree)
