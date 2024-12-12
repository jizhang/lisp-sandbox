(require 'ert)

(defmacro assert-error (form message)
  (let ((err (make-symbol "err")))
    `(let ((,err (should-error ,form)))
       (should (equal (cadr ,err) ,message)))))

(provide 'test-utils)
