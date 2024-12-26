(require 'ert)
(require 'doubly-linked-list)
(require 'test-utils)

(ert-deftest dlist-test-add ()
  (let ((dlist (make-dlist)))
    (should (equal (dlist-as-list dlist) nil))

    (should (equal (dlist-add dlist 1) t))
    (should (equal (dlist-as-list dlist) '(1)))

    (dlist-add dlist 2)
    (dlist-add dlist 3)
    (should (equal (dlist-as-list dlist) '(1 2 3)))
    (should (equal (dlist-as-list dlist t) '(3 2 1)))))

(ert-deftest dlist-test-add-at ()
  (let ((dlist (make-dlist)))
    (should (equal (dlist-add-first dlist 1) t))
    (should (equal (dlist-as-list dlist) '(1)))

    (dlist-add-first dlist 2)
    (should (equal (dlist-as-list dlist) '(2 1)))

    (dlist-add-at dlist 3 1)
    (should (equal (dlist-as-list dlist) '(2 3 1)))

    (dlist-add-at dlist 4 3)
    (should (equal (dlist-as-list dlist) '(2 3 1 4)))

    (let ((message "Index out of bounds"))
      (assert-error (dlist-add-at dlist 99 -1) message)
      (assert-error (dlist-add-at dlist 99 5) message))))

(ert-deftest dlist-test-add-all ()
  (let ((dlist (make-dlist)))
    (should (equal (dlist-add-all dlist nil) t))
    (should (equal (dlist-as-list dlist) nil))

    (dlist-add-all dlist '(1))
    (should (equal (dlist-as-list dlist) '(1)))

    (dlist-add-all dlist '(2 3))
    (should (equal (dlist-as-list dlist) '(1 2 3)))

    (should-error (dlist-add-all dlist 4) :type 'wrong-type-argument))

  (let* ((elements '(1 2 3))
         (dlist (make-dlist elements))
         (result (dlist-as-list dlist)))
    (should (equal result elements))))

(ert-deftest dlist-test-remove-at ()
  (let ((dlist (make-dlist))
        (no-such-element "No such element"))
    (assert-error (dlist-remove dlist) no-such-element)

    (dlist-add dlist 1)
    (dlist-add dlist 2)
    (should (equal (dlist-remove dlist) 1))
    (should (equal (dlist-as-list dlist) '(2)))

    (dlist-add dlist 3)
    (dlist-add dlist 4)
    (should (equal (dlist-remove dlist) 2))
    (should (equal (dlist-as-list dlist) '(3 4)))

    (dlist-add dlist 5)
    (should (equal (dlist-remove-at dlist 1) 4))
    (should (equal (dlist-remove-at dlist 1) 5))
    (should (equal (dlist-as-list dlist) '(3)))

    (assert-error (dlist-remove-at dlist 1) no-such-element)
    (assert-error (dlist-remove-at dlist -1) no-such-element)

    (dlist-remove dlist)
    (assert-error (dlist-remove dlist) no-such-element)))

(ert-deftest dlist-test-remove-last ()
  (let ((dlist (make-dlist))
        (no-such-element "No such element"))
    (assert-error (dlist-remove-last dlist) no-such-element)

    (dlist-add dlist 1)
    (dlist-add dlist 2)
    (should (equal (dlist-remove-last dlist) 2))
    (should (equal (dlist-as-list dlist) '(1)))

    (dlist-add dlist 3)
    (dlist-add dlist 4)
    (should (equal (dlist-remove-last dlist) 4))
    (should (equal (dlist-as-list dlist) '(1 3)))

    (should (equal (dlist-remove-last dlist) 3))
    (should (equal (dlist-remove-last dlist) 1))
    (assert-error (dlist-remove-last dlist) no-such-element)))

(ert-deftest dlist-test-clear ()
  (let ((dlist (make-dlist)))
    (dlist-add dlist 1)
    (dlist-add dlist 2)
    (should (equal (dlist-clear dlist) t))
    (should (equal (dlist-as-list dlist) nil))))

(ert-deftest dlist-test-index-of ()
  (let ((dlist (make-dlist)))
    (should (equal (dlist-index-of dlist 1) -1))

    (dlist-add dlist 1)
    (dlist-add dlist 2)
    (dlist-add dlist 3)
    (should (equal (dlist-index-of dlist 1) 0))
    (should (equal (dlist-index-of dlist 2) 1))
    (should (equal (dlist-index-of dlist 3) 2))
    (should (equal (dlist-index-of dlist 4) -1))

    (should (dlist-contains dlist 2))
    (should-not (dlist-contains dlist 4))

    (dlist-clear dlist)
    (should-not (dlist-contains dlist 2))))

(ert-deftest dlist-test-size ()
  (let ((dlist (make-dlist)))
    (should (equal (dlist-size dlist) 0))

    (dlist-add dlist 1)
    (should (equal (dlist-size dlist) 1))

    (dlist-add dlist 2)
    (should (equal (dlist-size dlist) 2))

    (dlist-clear dlist)
    (should (equal (dlist-size dlist) 0))))
