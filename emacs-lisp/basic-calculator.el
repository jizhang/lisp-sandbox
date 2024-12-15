(require 'cl-lib)
(require 'stack)
(require 'doubly-linked-list)

(defun bc-reverse-polish-notation (tokens)
  (let ((s (make-stack)))
    (dolist (token tokens (stack-pop s))
      (cond ((memq token '(+ - * /))
             (let ((b (stack-pop s))
                   (a (stack-pop s)))
               (stack-push s (funcall token a b))))
            ((eq token 'u)
             (stack-push s (- (stack-pop s))))
            (t
             (stack-push s token))))))

(defun bc-tokenize (input)
  (let ((tokens (make-dlist))
        (i 0))
    (while (< i (length input))
      (pcase (aref input i)
        ((pred cl-digit-char-p)
         (let ((num (- (aref input i) ?0)))
           (while (and (< (1+ i) (length input))
                       (cl-digit-char-p (aref input (1+ i))))
             (setq num (+ (* num 10) (- (aref input (cl-incf i)) ?0))))
           (add-to-dlist tokens num)))

        ((or ?+ ?- ?* ?/ ?\( ?\))
         (add-to-dlist tokens (intern (char-to-string (aref input i))))))

      (cl-incf i))

    (dlist-as-list tokens)))

(defun bc-convert-to-rpn (input)
  (let ((tokens (bc-tokenize input))
        (output (make-dlist))
        (s (make-stack))
        (previous-token nil))
    (dolist (token tokens)
      (cond
       ((memq token '(+ - * /))
        (let ((op (if (bc-unary-negation-p token previous-token) 'u token)))
          (while (and (memq (stack-peek s) '(+ - * / u))
                      (bc-operator-pop-p op (stack-peek s)))
            (add-to-dlist output (stack-pop s)))
          (stack-push s op)))

       ((eq token '\()
        (stack-push s token))

       ((eq token '\))
        (while (not (eq (stack-peek s) '\())
          (add-to-dlist output (stack-pop s)))
        (stack-pop s))

       (t
        (add-to-dlist output token)))

      (setq previous-token token))

    (while (> (stack-size s) 0)
      (add-to-dlist output (stack-pop s)))

    (dlist-as-list output)))

(defun bc-unary-negation-p (op previous-token)
  (and (eq op '-)
       (or (null previous-token)
           (memq previous-token '(+ - * / \()))))

(defun bc-operator-precedence (op)
  (cond ((memq op '(+ -)) 10)
        ((memq op '(* /)) 20)
        ((memq op '(u)) 30)
        (t (error "Unknown operator"))))

(defun bc-operator-associativity (op)
  (cond ((memq op '(+ - * /)) 'left)
        ((memq op '(u)) 'right)
        (t (error "Unknown operator"))))

(defun bc-operator-pop-p (op top)
  (let ((op-associativity (bc-operator-associativity op))
        (op-precedence (bc-operator-precedence op))
        (top-precedence (bc-operator-precedence top)))
    (cond ((eq op-associativity 'left)
           (<= op-precedence top-precedence))
          ((eq op-associativity 'right)
           (< op-precedence top-precedence))
          (t (cl-assert nil)))))

(defun bc-calculate (input)
  (let ((tokens (bc-convert-to-rpn input)))
    (bc-reverse-polish-notation tokens)))

(princ (bc-convert-to-rpn "-1") t)

(provide 'basic-calculator)
