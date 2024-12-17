(defsubst halve (n) (/ n 2))
(defsubst merge-sort-mid (p r) (+ p (halve (- r p))))

(defun merge-sort (nums p r)
  (when (< p r)
    (let ((q (merge-sort-mid p r)))
      (merge-sort nums p q)
      (merge-sort nums (1+ q) r)
      (merge-sort-merge nums p q r))))

(defun merge-sort-merge (nums p q r)
  (let* ((tmp (make-vector (1+ (- r p)) 0))
         (i p)
         (j (1+ q))
         (k 0))
    (while (and (<= i q) (<= j r))
      (let ((a (aref nums i))
            (b (aref nums j)))
        (cond ((<= a b)
               (aset tmp k a)
               (cl-incf i))
              (t
               (aset tmp k b)
               (cl-incf j))))
      (cl-incf k))

    (while (<= i q)
      (aset tmp k (aref nums i))
      (cl-incf i)
      (cl-incf k))

    (while (<= j r)
      (aset tmp k (aref nums j))
      (cl-incf j)
      (cl-incf k))

    (setq k 0)
    (while (< k (length tmp))
      (aset nums (+ p k) (aref tmp k))
      (cl-incf k))

    tmp))

(defun merge-sort-immutable (nums)
  (let ((len (length nums)))
    (if (<= len 1)
        nums
      (let* ((mid (halve len))
             (nums1 (merge-sort-immutable (substring nums 0 mid)))
             (nums2 (merge-sort-immutable (substring nums mid len))))
        (merge-sort-immutable-merge nums1 nums2 [])))))

(defun merge-sort-immutable-merge (nums1 nums2 result)
  (cond ((zerop (length nums1)) (vconcat result nums2))
        ((zerop (length nums2)) (vconcat result nums1))
        ((<= (aref nums1 0) (aref nums2 0))
         (merge-sort-immutable-merge (substring nums1 1) nums2
                                     (vconcat result (substring nums1 0 1))))
        (t
         (merge-sort-immutable-merge nums1 (substring nums2 1)
                                     (vconcat result (substring nums2 0 1))))))

(let ((nums [11 8 3 9 7 1 2 5]))
  (let ((nums (copy-sequence nums)))
    (merge-sort nums 0 (1- (length nums)))
    (princ nums t))
  (princ (merge-sort-immutable nums) t))
