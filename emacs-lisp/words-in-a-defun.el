(defun count-words-in-defun ()
  (beginning-of-defun)
  (let ((count 0)
        (end (save-excursion (end-of-defun) (point))))
    (while (and (< (point) end)
                (re-search-forward "\\(\\w\\|\\s_\\)+[^ \t\n]*[ \t\n]*" end t))
      (setq count (1+ count)))
    count))

(defun lengths-list-file (filename)
  (message "Working on `%s' ..." filename)
  (save-excursion
    (let ((buffer (find-file-noselect filename))
          lengths-list)
      (set-buffer buffer)
      (setq buffer-read-only t)
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^(defun" nil t)
        (setq lengths-list (cons (count-words-in-defun) lengths-list)))
      (kill-buffer buffer)
      lengths-list)))

(defun lengths-list-many-files (list-of-files)
  (let (lengths-list)
    (dolist (filename list-of-files lengths-list)
      (setq lengths-list (append lengths-list
                                 (lengths-list-file (expand-file-name filename)))))))

(defun recursive-lengths-list-many-files (list-of-files)
  (if list-of-files
      (append (lengths-list-file (expand-file-name (car list-of-files)))
              (recursive-lengths-list-many-files (cdr list-of-files)))
    nil))

(defun files-in-below-directory (dir)
  (let (el-files
        (files (directory-files-and-attributes dir t)))
    (dolist (file files el-files)
      (cond
       ((equal ".el" (substring (car file) -3))
        (setq el-files (cons (car file) el-files)))
       ((and (cadr file) ; Is directory
             (not (equal "." (substring (car file) -1))))
        (setq el-files (append (files-in-below-directory (car file)) el-files)))))))

(defun defuns-per-range (sorted-lengths top-of-ranges)
  (let ((top-of-range (car top-of-ranges))
        (number-within-range 0)
        defuns-per-range-list)
    (while top-of-ranges
      (while (and (car sorted-lengths)
                  (< (car sorted-lengths) top-of-range))
        (setq number-within-range (1+ number-within-range))
        (setq sorted-lengths (cdr sorted-lengths)))

      (setq defuns-per-range-list (cons number-within-range defuns-per-range-list))
      (setq number-within-range 0)

      (setq top-of-ranges (cdr top-of-ranges))
      (setq top-of-range (car top-of-ranges)))

    (setq defuns-per-range-list (cons (length sorted-lengths) defuns-per-range-list))
    (nreverse defuns-per-range-list)))
