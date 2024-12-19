(require 'cl-lib)
(require 'ert)
(require 'hash-map)

(ert-deftest hash-map-test-put ()
  (let ((map (hash-map-create)))
    (should (null (hash-map-get map 1)))
    (should (zerop (hash-map-size map)))

    (should (null (hash-map-put map 1 1)))
    (should (equal (hash-map-get map 1) 1))
    (should (equal (hash-map-size map) 1))

    (should (equal (hash-map-put map 1 2) 1))
    (should (equal (hash-map-get map 1) 2))
    (should (equal (hash-map-size map) 1))

    (cl-loop
     for i from 0 to 3
     for key = (* i 10)
     for size = 2 then (1+ size)
     do
     (should (null (hash-map-put map key i)))
     (should (equal (hash-map-get map key) i))
     (should (equal (hash-map-size map) size)))

    (dotimes (i 3) (should (equal (hash-map-get map (* i 10)) i)))))

(ert-deftest hash-map-test-remove ()
  (let ((map (hash-map-create)))
    (should (null (hash-map-remove map 1)))

    (hash-map-put map 1 1)
    (dotimes (i 4) (hash-map-put map (* i 10) i))

    (should (equal (hash-map-remove map 1) 1))
    (should (null (hash-map-remove map 1)))
    (should (equal (hash-map-size map) 4))

    (cl-loop
     for i in '(0 2 3 1)
     for key = (* i 10)
     for size = 3 then (1- size)
     do
     (should (equal (hash-map-remove map key) i))
     (should (null (hash-map-remove map key)))
     (should (equal (hash-map-size map) size)))))
