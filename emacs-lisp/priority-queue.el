(require 'cl-lib)

(cl-defstruct (priority-queue (:constructor priority-queue--create))
  pairs
  (size 0))

(defun priority-queue-create (max-size)
  (let ((pairs (make-vector (1+ max-size) nil)))
    (priority-queue--create :pairs pairs)))

(defun priority-queue-add (queue element priority)
  (let ((pairs (priority-queue-pairs queue)))
    (when (= (1- (length pairs)) (priority-queue-size queue))
      (error "Queue full"))

    (let ((current (cl-incf (priority-queue-size queue))))
      (while-let (((> current 1))
                  (parent (/ current 2))
                  (parent-pair (aref pairs parent))
                  ((> priority (cdr parent-pair))))
        (aset pairs current parent-pair)
        (setq current parent))

      (aset pairs current (cons element priority))))

  t)

(defun priority-queue-remove (queue)
  (when (zerop (priority-queue-size queue)) (error "No such element"))

  (let* ((pairs (priority-queue-pairs queue))
         (top-pair (aref pairs 1))
         (end (priority-queue-size queue))
         (end-pair (aref pairs end)))
    (aset pairs end nil)
    (when-let ((size (cl-decf (priority-queue-size queue)))
               ((> size 0))
               (half (/ size 2))
               (current 1)
               (priority (cdr end-pair)))
      (while-let (((<= current half))
                  (child (* current 2))
                  (child-priority (cdr (aref pairs child)))
                  (right (1+ child)))
        (when-let (((<= right size))
                   (right-priority (cdr (aref pairs right)))
                   ((> right-priority child-priority)))
          (setq child right
                child-priority right-priority))

        (if (> priority child-priority)
            (setq current size)
          (aset pairs current (aref pairs child))
          (setq current child)))

      (aset pairs current end-pair))

    top-pair))

(let ((queue (priority-queue-create 5)))
  (priority-queue-add queue 2 2)
  (princ (priority-queue-pairs queue) t))

(provide 'priority-queue)
