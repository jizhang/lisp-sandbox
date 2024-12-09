(require 'doubly-linked-list)

(defmacro assert-error (form message)
  (let ((err (make-symbol "err")))
    `(let ((,err (should-error ,form)))
       (should (equal (cadr ,err) ,message)))))

(ert-deftest dlist-test-add ()
  (let ((dlist (make-dlist)))
    (should (equal (dlist-as-list dlist) nil))

    (should (equal (add-to-dlist dlist 1) t))
    (should (equal (dlist-as-list dlist) '(1)))

    (add-to-dlist dlist 2)
    (add-to-dlist dlist 3)
    (should (equal (dlist-as-list dlist) '(1 2 3)))))

(ert-deftest dlist-test-add-at ()
  (let ((dlist (make-dlist)))
    (should (equal (add-first-to-dlist dlist 1) t))
    (should (equal (dlist-as-list dlist) '(1)))

    (add-first-to-dlist dlist 2)
    (should (equal (dlist-as-list dlist) '(2 1)))

    (add-to-dlist-at dlist 3 1)
    (should (equal (dlist-as-list dlist) '(2 3 1)))

    (add-to-dlist-at dlist 4 3)
    (should (equal (dlist-as-list dlist) '(2 3 1 4)))

    (let ((message "Index out of bounds"))
      (assert-error (add-to-dlist-at dlist 99 -1) message)
      (assert-error (add-to-dlist-at dlist 99 5) message))))

(ert-deftest dlist-test-remove-at ()
  (let ((dlist (make-dlist))
        (no-such-element "No such element"))
    (assert-error (remove-from-dlist dlist) no-such-element)

    (add-to-dlist dlist 1)
    (add-to-dlist dlist 2)
    (should (equal (remove-from-dlist dlist) 1))
    (should (equal (dlist-as-list dlist) '(2)))

    (add-to-dlist dlist 3)
    (add-to-dlist dlist 4)
    (should (equal (remove-from-dlist dlist) 2))
    (should (equal (dlist-as-list dlist) '(3 4)))

    (add-to-dlist dlist 5)
    (should (equal (remove-from-dlist-at dlist 1) 4))
    (should (equal (remove-from-dlist-at dlist 1) 5))
    (should (equal (dlist-as-list dlist) '(3)))

    (assert-error (remove-from-dlist-at dlist 1) no-such-element)
    (assert-error (remove-from-dlist-at dlist -1) no-such-element)

    (remove-from-dlist dlist)
    (assert-error (remove-from-dlist dlist) no-such-element)))

(ert-deftest dlist-test-remove-last ()
  (let ((dlist (make-dlist))
        (no-such-element "No such element"))
    (assert-error (remove-last-from-dlist dlist) no-such-element)

    (add-to-dlist dlist 1)
    (add-to-dlist dlist 2)
    (should (equal (remove-last-from-dlist dlist) 2))
    (should (equal (dlist-as-list dlist) '(1)))

    (add-to-dlist dlist 3)
    (add-to-dlist dlist 4)
    (should (equal (remove-last-from-dlist dlist) 4))
    (should (equal (dlist-as-list dlist) '(1 3)))

    (should (equal (remove-last-from-dlist dlist) 3))
    (should (equal (remove-last-from-dlist dlist) 1))
    (assert-error (remove-last-from-dlist dlist) no-such-element)))

(ert-deftest dlist-test-clear ()
  (let ((dlist (make-dlist)))
    (add-to-dlist dlist 1)
    (add-to-dlist dlist 2)
    (should (equal (clear-dlist dlist) t))
    (should (equal (dlist-as-list dlist) nil))))

(ert-deftest dlist-test-size ()
  (let ((dlist (make-dlist)))
    (should (equal (dlist-size dlist) 0))

    (add-to-dlist dlist 1)
    (should (equal (dlist-size dlist) 1))

    (add-to-dlist dlist 2)
    (should (equal (dlist-size dlist) 2))

    (clear-dlist dlist)
    (should (equal (dlist-size dlist) 0))))
