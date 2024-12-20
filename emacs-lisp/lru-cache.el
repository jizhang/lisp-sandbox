(require 'cl-lib)

(cl-defstruct (lru-cache-node (:constructor lru-cache-node-create))
  key value prev next hnext)

(cl-defstruct (lru-cache (:constructor lru-cache--create))
  capacity table head tail (size 0))

(defun lru-cache-create (capacity)
  (unless (and (integerp capacity) (> capacity 0))
    (error "Illegal argument"))

  (let ((table (make-vector capacity nil))
        (head (lru-cache-node-create))
        (tail (lru-cache-node-create)))
    (setf (lru-cache-node-next head) tail)
    (setf (lru-cache-node-prev tail) head)
    (lru-cache--create
     :capacity capacity
     :table table
     :head head
     :tail tail)))

(defun lru-cache-put (cache key value)
  (if-let ((current (lru-cache--get-table-node cache key)))
      (let ((old-value (lru-cache-node-value current)))
        (setf (lru-cache-node-value current) value)
        (lru-cache--remove-linked-node current)
        (lru-cache--push-linked-node cache current)
        old-value)

    (while-let (((>= (lru-cache-size cache) (lru-cache-capacity cache)))
                (tail (lru-cache-tail cache))
                (current (lru-cache-node-prev tail)))
      (lru-cache--remove-linked-node current)
      (lru-cache--remove-table-node cache current)
      (cl-decf (lru-cache-size cache)))

    (let ((current (lru-cache-node-create :key key :value value)))
      (lru-cache--push-linked-node cache current)
      (lru-cache--push-table-node cache current)
      (cl-incf (lru-cache-size cache))
      nil)))

(defun lru-cache-get (cache key)
  (if-let ((current (lru-cache--get-table-node cache key)))
      (progn
        (lru-cache--remove-linked-node current)
        (lru-cache--push-linked-node cache current)
        (lru-cache-node-value current))
    nil))

(defun lru-cache--slot (cache key)
  (let* ((hash (sxhash-equal key))
         (n (length (lru-cache-table cache))))
    (abs (% hash n))))

(cl-defun lru-cache--get-table-node (cache key)
  (let* ((table (lru-cache-table cache))
         (slot (lru-cache--slot cache key))
         (current (aref table slot)))
    (while (lru-cache-node-p current)
      (when (equal (lru-cache-node-key current) key)
        (cl-return-from lru-cache--get-table-node current))
      (setq current (lru-cache-node-hnext current)))

    nil))

(defun lru-cache--push-table-node (cache node)
  (let* ((table (lru-cache-table cache))
         (slot (lru-cache--slot cache (lru-cache-node-key node))))
    (setf (lru-cache-node-hnext node) (aref table slot))
    (aset table slot node)))

(cl-defun lru-cache--remove-table-node (cache node)
  (let* ((table (lru-cache-table cache))
         (slot (lru-cache--slot cache (lru-cache-node-key node)))
         (current (aref table slot))
         (prev nil))
    (while (lru-cache-node-p current)
      (let ((hnext (lru-cache-node-hnext current)))
        (when (eq current node)
          (if (lru-cache-node-p prev)
              (setf (lru-cache-node-hnext prev) hnext)
            (aset table slot hnext))
          (cl-return-from lru-cache--remove-table-node))

        (setq prev current)
        (setq current hnext)))

    (cl-assert nil)))

(defun lru-cache--push-linked-node (cache node)
  (let* ((head (lru-cache-head cache))
         (next (lru-cache-node-next head)))
    (setf (lru-cache-node-next head) node)
    (setf (lru-cache-node-prev node) head)
    (setf (lru-cache-node-next node) next)
    (setf (lru-cache-node-prev next) node)))

(defun lru-cache--remove-linked-node (node)
  (let ((prev (lru-cache-node-prev node))
        (next (lru-cache-node-next node)))
    (setf (lru-cache-node-next prev) next)
    (setf (lru-cache-node-prev next) prev)))

(let ((cache (lru-cache-create 2)))
  (lru-cache-put cache 1 1)
  (lru-cache-put cache 2 2)
  (lru-cache-put cache 3 3)
  (princ (lru-cache-get cache 1) t))

(provide 'lru-cache)
