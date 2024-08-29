(defvar graph-symbol "*")
(defvar graph-blank " ")

(defun column-of-graph (max-graph-height actual-height)
  (let ((number-of-top-blanks (- max-graph-height actual-height))
        insert-list)

    (while (> actual-height 0)
      (setq insert-list (cons graph-symbol insert-list))
      (setq actual-height (1- actual-height)))

    (while (> number-of-top-blanks 0)
      (setq insert-list (cons graph-blank insert-list))
      (setq number-of-top-blanks (1- number-of-top-blanks)))

    insert-list))

(defun graph-body-print (numbers-list)
  (let ((height (apply 'max numbers-list))
        (symbol-width (length graph-blank))
        from-position)

    (while numbers-list
      (setq from-position (point))
      (insert-rectangle (column-of-graph height (car numbers-list)))
      (goto-char from-position)
      (forward-char symbol-width)
      (sit-for 0)
      (setq numbers-list (cdr numbers-list)))

    (forward-line height)
    (insert "\n")))

(defun recursive-graph-body-print (numbers-list)
  (let ((height (apply 'max numbers-list))
        (symbol-width (length graph-blank)))
    (recursive-graph-body-print-internal numbers-list height symbol-width)))

(defun recursive-graph-body-print-internal (numbers-list height symbol-width)
  (when numbers-list
    (save-excursion
      (insert-rectangle (column-of-graph height (car numbers-list))))
    (forward-char symbol-width)
    (sit-for 0)
    (recursive-graph-body-print-internal (cdr numbers-list) height symbol-width)))
