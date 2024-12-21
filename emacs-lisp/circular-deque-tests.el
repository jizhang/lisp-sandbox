(require 'ert)
(require 'circular-deque)
(require 'test-utils)

(ert-deftest deque-test-add ()
  (let ((deque (deque-create 3)))
    (should (deque-empty-p deque))

    (should (deque-add-first deque 1))
    (should (deque-add-last deque 2))
    (should (equal (deque-as-list deque) '(1 2)))
    (should (equal (deque-size deque) 2))
    (should-not (deque-empty-p deque))
    (should-not (deque-full-p deque))

    (deque-add-last deque 3)
    (should (deque-full-p deque))
    (assert-error (deque-add-last deque 4) "Deque full")
    (assert-error (deque-add-first deque 0) "Deque full")))

(ert-deftest deque-test-remove ()
  (let ((deque (deque-create 3)))
    (deque-add-last deque 1)
    (deque-add-last deque 2)
    (deque-add-last deque 3)

    (should (equal (deque-remove-first deque) 1))
    (should (equal (deque-remove-last deque) 3))
    (should (equal (deque-as-list deque) '(2)))

    (deque-remove-first deque)
    (should (zerop (deque-size deque)))
    (assert-error (deque-remove-first deque) "No such element")
    (assert-error (deque-remove-last deque) "No such element")))

(ert-deftest deque-test-circular ()
  (let ((deque (deque-create 3)))
    (deque-add-last deque 1)
    (deque-add-last deque 2)
    (deque-add-last deque 3)

    (deque-remove-first deque)
    (deque-add-last deque 4)
    (should (equal (deque-as-list deque) '(2 3 4)))

    (deque-remove-last deque)
    (should (equal (deque-as-list deque) '(2 3)))

    (deque-remove-last deque)
    (deque-add-first deque 1)
    (deque-add-first deque 0)
    (should (equal (deque-as-list deque) '(0 1 2)))

    (deque-remove-first deque)
    (should (equal (deque-as-list deque) '(1 2)))))

(ert-deftest deque-test-peek ()
  (let ((deque (deque-create 3)))
    (deque-add-first deque 3)
    (deque-add-first deque 2)
    (deque-add-first deque 1)
    (should (equal (deque-peek-first deque) 1))
    (should (equal (deque-peek-last deque) 3))))
