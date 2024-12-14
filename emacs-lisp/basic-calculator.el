(require 'cl-lib)
(require 'stack)
(require 'doubly-linked-list)

(defun reverse-polish-notation (tokens)
  (princ tokens t) ; FIXME
  (let ((s (make-stack)))
    (dolist (token tokens (stack-pop s))
      (if (memq token '(+ - * /))
          (let ((b (stack-pop s))
                (a (stack-pop s)))
            (stack-push s (funcall token a b)))
        (stack-push s token)))))

(defun basic-calculator (input)
  (let ((output (make-dlist))
        (s (make-stack))
        (i 0))
    (while (< i (length input))
      (pcase (aref input i)
        ((pred cl-digit-char-p)
         (let ((num (- (aref input i) ?0)))
           (while (and (< (1+ i) (length input))
                       (cl-digit-char-p (aref input (1+ i))))
             (setq num (+ (* num 10) (- (aref input (cl-incf i)) ?0))))
           (add-to-dlist output num)))

        ((or ?+ ?- ?* ?/)
         (let ((op (intern (char-to-string (aref input i))))
               (top (stack-peek s)))
           (when (memq top '(+ - * /))
             (unless (and (memq op '(* /)) (memq top '(+ -)))
               (add-to-dlist output (stack-pop s))))
           (stack-push s op)))

        (?\(
         (stack-push s (aref input i)))

        (?\)
         (while (not (eq (stack-peek s) ?\())
           (add-to-dlist output (stack-pop s)))
         (stack-pop s)))

      (cl-incf i))

    (while (> (stack-size s) 0)
      (add-to-dlist output (stack-pop s)))

    (reverse-polish-notation (dlist-as-list output))))

(princ (basic-calculator "(((3 + 3) / 2))") t)

;; TODO
;; Negative expressions: -10 + 1, -(10 + 1)
;; Calculate directly.
;; Use recursion.

(provide 'basic-calculator)
