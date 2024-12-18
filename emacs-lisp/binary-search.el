(require 'cl-lib)

(defsubst binsearch-mid (low high)
  (+ low (/ (- high low) 2)))

(defun binary-search (nums target)
  (let ((low 0)
        (high (1- (length nums)))
        (result -1))
    (while (and (<= low high) (= result -1))
      (let* ((mid (binsearch-mid low high))
             (mid-value (aref nums mid)))
        (cond ((< target mid-value)
               (setq high (1- mid)))
              ((> target mid-value)
               (setq low (1+ mid)))
              (t
               (setq result mid)))))
    result))

(cl-defun binary-search-iter (nums target &optional (low 0) (high (1- (length nums))))
  (if (> low high)
      -1
    (let* ((mid (binsearch-mid low high))
           (mid-value (aref nums mid)))
      (cond ((< target mid-value)
             (binary-search-iter nums target low (1- mid)))
            ((> target mid-value)
             (binary-search-iter nums target (1+ mid) high))
            (t mid)))))

(cl-defun binsearch-find-first (nums target)
  (let ((low 0)
        (high (1- (length nums))))
    (while (<= low high)
      (let* ((mid (binsearch-mid low high))
             (mid-value (aref nums mid)))
        (cond ((< target mid-value)
               (setq high (1- mid)))
              ((> target mid-value)
               (setq low (1+ mid)))
              ((or (zerop mid) (> target (aref nums (1- mid))))
               (cl-return-from binsearch-find-first mid))
              (t
               (setq high (1- mid)))))))
  -1)

(defun binsearch-find-first-gte (nums target)
  (let ((low 0)
        (high (1- (length nums))))
    (cl-block nil
      (while (<= low high)
        (let* ((mid (binsearch-mid low high))
               (mid-value (aref nums mid)))
          (cond ((> target mid-value)
                 (setq low (1+ mid)))
                ((or (zerop mid) (> target (aref nums (1- mid))))
                 (cl-return mid))
                (t
                 (setq high (1- mid))))))
      -1)))

(defun binsearch-find-last (nums target)
  (cl-loop
   with max = (1- (length nums))
   with low = 0
   with high = max
   while (<= low high)
   for mid = (binsearch-mid low high)
   for mid-value = (aref nums mid)
   if (< target mid-value) do (setq high (1- mid))
   else if (> target mid-value) do (setq low (1+ mid))
   else if (or (= mid max) (< target (aref nums (1+ mid)))) return mid
   else do (setq low (1+ mid))
   finally return -1))

(defun binsearch-find-last-lte (nums target)
  (cl-loop
   with max = (1- (length nums))
   with low = 0
   with high = max
   while (<= low high)
   for mid = (binsearch-mid low high)
   for mid-value = (aref nums mid)
   if (< target mid-value) do (setq high (1- mid))
   else if (or (= mid max) (< target (aref nums (1+ mid)))) return mid
   else do (setq low (1+ mid))
   finally return -1))

(let ((nums [1 3 4 5 6 8 8 8 11 18]))
  (princ (binsearch-find-last-lte nums 9) t))

(provide 'binary-search)
