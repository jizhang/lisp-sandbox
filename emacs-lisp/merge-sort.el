(defsubst halve (n) (/ n 2))
(defsubst merge-sort-mid (p r) (+ p (halve (- r p))))

(defun merge-sort (nums p r)
  (when (< p r)
    (let ((q (merge-sort-mid p r)))
      (merge-sort nums p q)
      (merge-sort nums (1+ q) r)
      (merge-sort-merge nums p q r))))

(defun merge-sort-merge (nums p q r)
  (let* ((range1 (cons p q))
         (range2 (cons (1+ q) r))
         (tmp (merge-sort-merge-immutable nums nums range1 range2)))
    (dotimes (k (length tmp))
      (aset nums (+ p k) (aref tmp k)))))

(defun merge-sort-merge-immutable (nums1 nums2 &optional range1 range2)
  (unless (consp range1) (setq range1 (cons 0 (1- (length nums1)))))
  (unless (consp range2) (setq range2 (cons 0 (1- (length nums2)))))

  (let* ((i (car range1))
         (q (cdr range1))
         (len1 (1+ (- q i)))
         (j (car range2))
         (r (cdr range2))
         (len2 (1+ (- r j)))
         (res (make-vector (+ len1 len2) 0))
         (k 0))

    (while (and (<= i q) (<= j r))
      (let ((a (aref nums1 i))
            (b (aref nums2 j)))
        (cond ((<= a b)
               (aset res k a)
               (cl-incf i))
              (t
               (aset res k b)
               (cl-incf j))))
      (cl-incf k))

    (while (<= i q)
      (aset res k (aref nums1 i))
      (cl-incf i)
      (cl-incf k))

    (while (<= j r)
      (aset res k (aref nums2 j))
      (cl-incf j)
      (cl-incf k))

    res))

(cl-defun merge-sort-immutable (nums &optional (p 0) (r (1- (length nums))))
  (cond ((> p r) (vector))
        ((= p r) (vector (aref nums p)))
        (t
         (let* ((q (merge-sort-mid p r))
                (nums1 (merge-sort-immutable nums p q))
                (nums2 (merge-sort-immutable nums (1+ q) r)))
           (merge-sort-merge-immutable nums1 nums2)))))

(defun merge-sort-immutable-1 (nums)
  (let ((len (length nums)))
    (if (<= len 1)
        nums
      (let* ((mid (halve len))
             (nums1 (merge-sort-immutable-1 (substring nums 0 mid)))
             (nums2 (merge-sort-immutable-1 (substring nums mid len))))
        (merge-sort-merge-immutable nums1 nums2)))))

(let ((nums (vector 11 8 3 9 7 1 2 5)))
  (merge-sort nums 0 (1- (length nums)))
  (princ nums t)
  (princ (merge-sort-immutable nums) t)
  (princ (merge-sort-immutable-1 nums) t))
