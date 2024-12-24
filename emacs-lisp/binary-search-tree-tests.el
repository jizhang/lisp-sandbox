(require 'ert)
(require 'binary-search-tree)
(require 'test-utils)

(ert-deftest bst-test-insert ()
  (dolist (fn '(bst-insert bst-insert-immutable))
    (let ((bst (bst-create)))
      (should (equal (bst-to-list bst) '()))

      (funcall fn bst 3)
      (should (equal (bst-to-list bst) '(3)))

      (funcall fn bst 2)
      (funcall fn bst 4)
      (should (equal (bst-to-list bst) '(2 3 4)))

      (funcall fn bst 5)
      (funcall fn bst 1)
      (should (equal (bst-to-list bst) '(1 2 3 4 5))))))

(ert-deftest bst-test-remove ()
  (dolist (fn '(bst-remove bst-remove-immutable))
    (let ((bst (bst-create)))
      (dolist (value '(2 1 4 3 5))
        (bst-insert bst value))

      (funcall fn bst 2)
      (should (equal (bst-to-list bst) '(1 3 4 5)))
      (should (equal (bst-node-value (bst-root bst)) 3))

      (funcall fn bst 4)
      (should (equal (bst-to-list bst) '(1 3 5)))

      (funcall fn bst 5)
      (should (equal (bst-to-list bst) '(1 3)))

      (assert-error (funcall fn bst 99) "No such element")

      (bst-insert bst 3)
      (bst-insert bst 3)
      (should (equal (bst-to-list bst) '(1 3 3 3)))
      (bst-remove bst 3)
      (should (equal (bst-to-list bst) '(1 3 3))))))
