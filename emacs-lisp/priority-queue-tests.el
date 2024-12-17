(require 'ert)
(require 'priority-queue)
(require 'test-utils)

(defun priority-queue--assert-pairs (queue expected-elements)
  (let* ((size (priority-queue-size queue))
         (pairs (priority-queue-pairs queue))
         (non-nil-pairs (substring pairs 1 (1+ size)))
         (elements (mapcar #'car non-nil-pairs)))
    (should (equal elements expected-elements))))

(ert-deftest priority-queue-test-add ()
  (let ((queue (priority-queue-create 7)))
    (priority-queue--assert-pairs queue nil)
    (should (equal (priority-queue-size queue) 0))

    (should (priority-queue-add queue 2 2))
    (priority-queue--assert-pairs queue '(2))
    (should (equal (priority-queue-size queue) 1))

    (priority-queue-add queue 3 3)
    (priority-queue-add queue 1 1)
    (priority-queue--assert-pairs queue '(3 2 1))
    (should (equal (priority-queue-size queue) 3))

    (priority-queue-add queue 4 4)
    (priority-queue-add queue 5 5)
    (priority-queue--assert-pairs queue '(5 4 1 2 3))

    (priority-queue-add queue 7 7)
    (priority-queue-add queue 6 6)
    (priority-queue--assert-pairs queue '(7 4 6 2 3 1 5))

    (assert-error (priority-queue-add queue 8 8) "Queue full")))

(ert-deftest priority-queue-test-remove ()
  (let ((queue (priority-queue-create 5)))
    (dotimes (n 5) (priority-queue-add queue (- 5 n) (- 5 n)))
    (priority-queue--assert-pairs queue '(5 4 3 2 1))
    (should (equal (priority-queue-size queue) 5))

    (should (equal (priority-queue-remove queue) '(5 . 5)))
    (priority-queue--assert-pairs queue '(4 2 3 1))

    (should (equal (priority-queue-remove queue) '(4 . 4)))
    (priority-queue--assert-pairs queue '(3 2 1))

    (dotimes (_ 3) (priority-queue-remove queue))
    (assert-error (priority-queue-remove queue) "No such element")
    (should (equal (priority-queue-pairs queue) (make-vector 6 nil)))))

(ert-deftest priority-queue-test-peek ()
  (let ((queue (priority-queue-create 3)))
    (should (null (priority-queue-peek queue)))

    (priority-queue-add queue 2 2)
    (should (equal (priority-queue-peek queue) '(2 . 2)))

    (priority-queue-add queue 1 1)
    (should (equal (priority-queue-peek queue) '(2 . 2)))

    (priority-queue-add queue 3 3)
    (priority-queue-remove queue)
    (should (equal (priority-queue-peek queue) '(2 . 2)))))
