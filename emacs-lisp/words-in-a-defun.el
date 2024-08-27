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
