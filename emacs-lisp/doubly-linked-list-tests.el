(require 'doubly-linked-list)

(defmacro assert-error (form message)
  `(let ((e (should-error ,form)))
     (should (equal (cadr e) ,message))))

(ert-deftest dlist-test-add ()
  (let ((dlist (make-dlist)))
    (should (equal (dlist-as-list dlist) nil))

    (add-to-dlist dlist 1)
    (should (equal (dlist-as-list dlist) '(1)))

    (add-to-dlist dlist 2)
    (add-to-dlist dlist 3)
    (should (equal (dlist-as-list dlist) '(1 2 3)))))

(ert-deftest dlist-test-add-at ()
  (let ((dlist (make-dlist)))
    (add-to-dlist-at dlist 0 1)
    (should (equal (dlist-as-list dlist) '(1)))

    (add-to-dlist-at dlist 0 2)
    (should (equal (dlist-as-list dlist) '(2 1)))

    (add-to-dlist-at dlist 1 3)
    (should (equal (dlist-as-list dlist) '(2 3 1)))

    (add-to-dlist-at dlist 3 4)
    (should (equal (dlist-as-list dlist) '(2 3 1 4)))

    (let ((message "Index out of bounds"))
      (assert-error (add-to-dlist-at dlist -1 99) message)
      (assert-error (add-to-dlist-at dlist 5 99) message))))

(ert-deftest dlist-test-remove ()
  (let ((dlist (make-dlist)))
    (add-to-dlist dlist 1)
    (add-to-dlist dlist 2)
    (should (equal (remove-from-dlist dlist) 1))
    (should (equal (dlist-as-list dlist) '(2)))

    (add-to-dlist dlist 3)
    (add-to-dlist dlist 4)
    (should (equal (remove-from-dlist dlist) 2))
    (should (equal (dlist-as-list dlist) '(3 4)))

    (remove-from-dlist dlist)
    (remove-from-dlist dlist)
    (assert-error (remove-from-dlist dlist) "No such element")))

(ert-deftest dlist-test-clear ()
  (let ((dlist (make-dlist)))
    (add-to-dlist dlist 1)
    (add-to-dlist dlist 2)
    (clear-dlist dlist)
    (should (equal (dlist-as-list dlist) nil))))
