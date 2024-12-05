(require 'doubly-linked-list)

(ert-deftest dlist-test-add ()
  (let ((dlist (make-dlist)))
    (should (equal (dlist-as-list dlist) nil))

    (add-to-dlist dlist 1)
    (should (equal (dlist-as-list dlist) '(1)))

    (add-to-dlist dlist 2)
    (add-to-dlist dlist 3)
    (should (equal (dlist-as-list dlist) '(1 2 3)))))

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
    (should-error (remove-from-dlist dlist))))

(ert-deftest dlist-test-clear ()
  (let ((dlist (make-dlist)))
    (add-to-dlist dlist 1)
    (add-to-dlist dlist 2)
    (clear-dlist dlist)
    (should (equal (dlist-as-list dlist) nil))))
