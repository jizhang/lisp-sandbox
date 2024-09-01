(defvar graph-symbol "*")
(defvar graph-blank " ")
(defvar Y-axis-label-spacing 5)
(defvar Y-axis-tic " - ")
(defvar X-axis-label-spacing (if (boundp 'graph-blank) (* 5 (length graph-blank)) 5))
(defvar X-axis-tic-symbol "|")

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

(defun graph-body-print (numbers-list height symbol-width)
  (while numbers-list
    (save-excursion
      (insert-rectangle (column-of-graph height (car numbers-list))))
    (forward-char symbol-width)
    (sit-for 0)
    (setq numbers-list (cdr numbers-list)))

  (forward-line height)
  (insert "\n"))

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

(defun Y-axis-element (number full-Y-label-width)
  (let* ((number-length (length (number-to-string number)))
         (tic-length (length Y-axis-tic))
         (leading-spaces (- full-Y-label-width number-length tic-length)))
    (concat
     (make-string leading-spaces ? )
     (number-to-string number)
     Y-axis-tic)))

(defun Y-axis-column (height full-Y-label-width vertical-step)
  (let (Y-axis)
    (while (> height 1)
      (let ((element
             (if (zerop (% height Y-axis-label-spacing))
                 (Y-axis-element (* height vertical-step) full-Y-label-width)
               (make-string full-Y-label-width ? ))))
        (setq Y-axis (cons element Y-axis)))
      (setq height (1- height)))

    (let ((element (Y-axis-element vertical-step full-Y-label-width)))
      (setq Y-axis (cons element Y-axis)))
    (nreverse Y-axis)))

(defun print-Y-axis (height full-Y-label-width vertical-step)
  (save-excursion
    (insert-rectangle (Y-axis-column height full-Y-label-width vertical-step)))
  (forward-char full-Y-label-width))

(defun print-X-axis-tic-line (leading-spaces symbol-width tic-count tic-element)
  (insert leading-spaces X-axis-tic-symbol)
  (let* ((space-length
          (- (* symbol-width X-axis-label-spacing)
             (* 2 (length X-axis-tic-symbol))))
         (spaces (make-string space-length ? )))
    (insert spaces X-axis-tic-symbol))

  (while (> tic-count 1)
    (insert tic-element)
    (setq tic-count (1- tic-count))))

(defun X-axis-element (number symbol-width)
  (let* ((space-length
          (- (* symbol-width X-axis-label-spacing)
             (length (number-to-string number))))
         (spaces (make-string space-length ? )))
    (concat spaces (number-to-string number))))

(defun print-X-axis-numbered-line (leading-spaces symbol-width tic-count)
  (insert leading-spaces "1")
  (let* ((space-length (- (* symbol-width X-axis-label-spacing) 2))
         (spaces (make-string space-length ? )))
    (insert spaces (number-to-string X-axis-label-spacing)))

  (let ((number (* X-axis-label-spacing 2)))
    (while (> tic-count 1)
      (insert (X-axis-element number symbol-width))
      (setq number (+ number X-axis-label-spacing))
      (setq tic-count (1- tic-count)))))

(defun print-X-axis (numbers-list symbol-width full-Y-label-width)
  (let* ((leading-spaces (make-string full-Y-label-width ? ))
         (X-tic-space-length
          (- (* symbol-width X-axis-label-spacing) (length X-axis-tic-symbol)))
         (X-tic (concat (make-string X-tic-space-length ? ) X-axis-tic-symbol))
         (X-length (length numbers-list))
         (tic-width (* symbol-width X-axis-label-spacing))
         (tic-number
          (if (zerop (% X-length tic-width))
              (/ X-length tic-width)
            (1+ (/ X-length tic-width)))))
    (print-X-axis-tic-line leading-spaces symbol-width tic-number X-tic)
    (insert "\n")
    (print-X-axis-numbered-line leading-spaces symbol-width tic-number)))

(defun print-graph (numbers-list &optional vertical-step)
  (let* ((symbol-width (length graph-blank))
         (height (apply 'max numbers-list))
         (height-of-top-line
          (if (zerop (% height Y-axis-label-spacing))
              height
            (* (1+ (/ height Y-axis-label-spacing)) Y-axis-label-spacing)))
         (vertical-step (or vertical-step 1))
         (max-Y-label (* height-of-top-line vertical-step))
         (full-Y-label (concat (number-to-string max-Y-label) Y-axis-tic))
         (full-Y-label-width (length full-Y-label)))
    (print-Y-axis height-of-top-line full-Y-label-width vertical-step)
    (graph-body-print numbers-list height-of-top-line symbol-width)
    (print-X-axis numbers-list symbol-width full-Y-label-width)))

;; (print-graph '(3 2 5 6 7 5 3 4 6 4 3 2 1) 2)
