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
    (aset pairs (cl-incf (priority-queue-size queue)) (cons element priority))

    (let ((current (priority-queue-size queue)))
      (while-let ((parent (/ current 2))
                  ((> parent 0))
                  ((> priority (cdr (aref pairs parent)))))
        (priority-queue--swap pairs current parent)
        (setq current parent))))
  t)

(defun priority-queue-remove (queue)
  (when (zerop (priority-queue-size queue)) (error "No such element"))
  (let* ((pairs (priority-queue-pairs queue))
         (top (aref pairs 1))
         (end (priority-queue-size queue)))
    (aset pairs 1 (aref pairs end))
    (aset pairs end nil)
    (cl-decf (priority-queue-size queue))

    (let* ((size (priority-queue-size queue))
           (half (/ size 2))
           (current 1)
           (priority (cdr (aref pairs current))))
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
          (priority-queue--swap pairs current child)
          (setq current child))))

    top))

(defun priority-queue--swap (pairs a b)
  (let ((tmp (aref pairs a)))
    (aset pairs a (aref pairs b))
    (aset pairs b tmp)))

(let ((queue (priority-queue-create 5)))
  (priority-queue-add queue 1 1)
  (priority-queue-add queue 2 2)
  (princ (priority-queue-remove queue) t))

(provide 'priority-queue)
