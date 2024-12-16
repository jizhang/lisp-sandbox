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
      (error "Priority queue is full"))
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
         (top (aref pairs 1)))
    (aset pairs 1 (aref pairs (priority-queue-size queue)))
    (aset pairs (priority-queue-size queue) nil)
    (cl-decf (priority-queue-size queue))

    (cl-block nil
      (let* ((size (priority-queue-size queue))
             (current 1))
        (while (<= current size)
          (let* ((left (* current 2))
                 (right (1+ left))
                 (pos current))
            (when (and (<= left size)
                       (> (cdr (aref pairs left)) (cdr (aref pairs pos))))
              (setq pos left))
            (when (and (<= right size)
                       (> (cdr (aref pairs right)) (cdr (aref pairs pos))))
              (setq pos right))

            (if (= current pos)
                (cl-return)
              (priority-queue--swap pairs current pos)
              (setq current pos))))))

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
