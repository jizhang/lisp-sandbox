(require 'ert)
(require 'lru-cache)

(ert-deftest lru-cache-test ()
  (let ((cache (lru-cache-create 3)))
    (should (null (lru-cache-get cache 1)))
    (should (zerop (lru-cache-size cache)))

    (should (null (lru-cache-put cache 1 1)))
    (should (equal (lru-cache-put cache 1 2) 1))
    (should (equal (lru-cache-get cache 1) 2))
    (should (equal (lru-cache-size cache) 1))

    (lru-cache-put cache 3 3)
    (lru-cache-put cache 6 6)
    (lru-cache-put cache 9 9)
    (should (equal (lru-cache-get cache 6) 6))
    (should (equal (lru-cache-size cache) 3))
    (should (null (lru-cache-get cache 1)))

    (lru-cache-put cache 1 1)
    (should (null (lru-cache-get cache 3)))))
