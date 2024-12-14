(require 'ert)

(defmacro assert-error (form message)
  (let ((err (make-symbol "err")))
    `(let ((,err (should-error ,form)))
       (should (equal (cadr ,err) ,message)))))

(defun assert-cases (fn cases)
  (dolist (case cases)
    (should (equal (funcall fn (car case)) (cdr case)))))

(provide 'test-utils)
