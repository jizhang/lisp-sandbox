(require 'cl-lib)

(cl-defstruct (bst-node (:constructor bst-node-create))
  value left right)

(cl-defun bst-node-replace (node &key (value (bst-node-value node))
                                 (left (bst-node-left node))
                                 (right (bst-node-right node)))
  (bst-node-create :value value :left left :right right))

(cl-defstruct (bst (:constructor bst-create))
  root)

(cl-defun bst-insert (bst value)
  (let ((node (bst-root bst))
        (new-node (bst-node-create :value value)))
    (when (null node)
      (setf (bst-root bst) new-node)
      (cl-return-from bst-insert))

    (while (bst-node-p node)
      (cond
       ((< value (bst-node-value node))
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
         (cond
          ((null node) (bst-node-create :value value))
          ((< value (bst-node-value node))
           (bst-node-replace
            node :left (insert (bst-node-left node) value)))
          (t
           (bst-node-replace
            node :right (insert (bst-node-right node) value))))))
    (setf (bst-root bst) (insert (bst-root bst) value))))

(defun bst-remove (bst value)
  (let ((node (bst-root bst))
        (parent nil))
    (while (and (bst-node-p node) (/= value (bst-node-value node)))
      (setf parent node)
      (if (< value (bst-node-value node))
          (setf node (bst-node-left node))
        (setf node(bst-node-right node))))

    (unless (bst-node-p node) (error "No such element"))

    (if (and (bst-node-p (bst-node-left node))
             (bst-node-p (bst-node-right node)))

        (let ((min-node (bst-node-right node))
              (min-node-parent node))
          (while (bst-node-p (bst-node-left min-node))
            (setf min-node-parent min-node)
            (setf min-node (bst-node-left min-node)))

          (setf (bst-node-value node) (bst-node-value min-node))
          (setf node min-node)
          (setf parent min-node-parent)))

    (let ((child (or (bst-node-left node) (bst-node-right node))))
      (cond
       ((null parent)
        (setf (bst-root bst) child))
       ((eq node (bst-node-left parent))
        (setf (bst-node-left parent) child))
       (t
        (setf (bst-node-right parent) child))))))

(defun bst-remove-immutable (bst value)
  (cl-labels
      ((find-min (node)
         (if-let ((left (bst-node-left node)))
             (find-min left)
           (bst-node-value node)))
       (remove (node value)
         (cond
          ((null node) (error "No such element"))
          ((< value (bst-node-value node))
           (bst-node-replace node :left (remove (bst-node-left node) value)))
          ((> value (bst-node-value node))
           (bst-node-replace node :right (remove (bst-node-right node) value)))
          ((null (bst-node-left node))
           (bst-node-right node))
          ((null (bst-node-right node))
           (bst-node-left node))
          (t
           (let* ((right (bst-node-right node))
                  (min-value (find-min right))
                  (new-right (remove (bst-node-right node) min-value)))
             (bst-node-replace node :value min-value :right new-right))))))
    (setf (bst-root bst) (remove (bst-root bst) value))))

(defun bst-to-list (bst)
  (let ((result nil))
    (cl-labels
        ((traverse (node)
           (when (bst-node-p node)
             (traverse (bst-node-left node))
             (push (bst-node-value node) result)
             (traverse (bst-node-right node)))))
      (traverse (bst-root bst)))
    (reverse result)))

(defun bst-height (bst)
  (cl-labels
      ((height (node)
         (if (null node)
             0
           (1+ (max (height (bst-node-left node))
                    (height (bst-node-right node)))))))
    (height (bst-root bst))))

(defun bst-to-vector (bst)
  (let* ((height (bst-height bst))
         (num-nodes (1- (expt 2 height)))
         (result (make-vector (1+ num-nodes) nil)))
    (cl-labels
        ((dfs (node index)
           (when (bst-node-p node)
             (aset result index (bst-node-value node))
             (dfs (bst-node-left node) (* index 2))
             (dfs (bst-node-right node) (1+ (* index 2))))))
      (dfs (bst-root bst) 1))
    result))

(let ((bst (bst-create)))
  (dolist (value '(2 1 4 3 5)) (bst-insert bst value))
  (bst-remove-immutable bst 2)
  (princ (bst-to-vector bst) t))

(provide 'binary-search-tree)
