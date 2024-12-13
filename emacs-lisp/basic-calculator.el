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
        (num nil)
        (s (make-stack)))
    (dolist (char (append input nil))
      (pcase char
        ((guard (and (>= char ?0) (<= char ?9)))
         (if (null num)
             (setq num (- char ?0))
           (setq num (+ (* num 10) (- char ?0)))))

        ((or ?+ ?- ?* ?/)
         (unless (null num)
           (add-to-dlist output num)
           (setq num nil))
         (let ((op (intern (char-to-string char)))
               (top (stack-peek s)))
           (when (memq top '(+ - * /))
             (unless (and (memq op '(* /)) (memq top '(+ -)))
               (add-to-dlist output (stack-pop s))))
           (stack-push s op)))

        (?\(
         (stack-push s char))

        (?\)
         (unless (null num)
           (add-to-dlist output num)
           (setq num nil))
         (while (not (eq (stack-peek s) ?\())
           (add-to-dlist output (stack-pop s)))
         (stack-pop s))))

    (unless (null num)
      (add-to-dlist output num)
      (setq num nil))
    (while (> (stack-size s) 0)
      (add-to-dlist output (stack-pop s)))

    (reverse-polish-notation (dlist-as-list output))))

(princ (basic-calculator "(((3 + 3) / 2))") t)

;; TODO
;; Negative expressions: -10 + 1, -(10 + 1)
;; Calculate directly.
;; Use recursion.
;; Parse number eagerly.

(provide 'basic-calculator)
