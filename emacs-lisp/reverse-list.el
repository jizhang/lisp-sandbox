(require 'cl-lib)

(defun reverse-list (head)
  (let ((prev nil)
        (current head))
    (while (consp current)
      (setq prev (cons (car current) prev))
      (setq current (cdr current)))
    prev))

(defun reverse-list-dolist (head)
  (let ((new nil))
    (dolist (value head new)
      (setq new (cons value new)))))

(defun reverse-list-destructive (head)
  (let ((prev nil)
        (current head))
    (while (consp current)
      (let ((next (cdr current)))
        (setcdr current prev)
        (setq prev current)
        (setq current next)))
    prev))

(defun reverse-list-recursive (head)
  (if (or (null head) (null (cdr head)))
      head
    (let ((rest (reverse-list-recursive (cdr head))))
      (setcdr (cdr head) head)
      (setcdr head nil)
      rest)))

(defun reverse-list-iter (result rest)
  "`(reverse-list nil head)'"
  (if (null rest)
      result
    (reverse-list-iter (cons (car rest) result) (cdr rest))))

(defun reverse-list-reduce (head)
  (cl-reduce
   (lambda (result item) (cons item result))
   head
   :initial-value nil))

(defun reverse-list-reduce-right (head)
  (cl-reduce
   (lambda (item result) (append result (list item)))
   head
   :initial-value nil
   :from-end t))

(let* ((head (list 1 2 3 4 5))
       (result (reverse-list-reduce-right head)))
  (princ result t))
