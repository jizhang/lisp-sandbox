(require 'ert)
(require 'basic-calculator)
(require 'test-utils)

(ert-deftest basic-calculator-test-reverse-polish-notation ()
  (let ((cases '(((2 1 + 3 *) . 9)
                 ((4 13 5 / +) . 6)
                 ((10 6 9 3 + -11 * / * 17 + 5 +) . 22))))
    (dolist (case cases)
      (should (equal (reverse-polish-notation (car case)) (cdr case))))))


(ert-deftest basic-calculator-convert-to-rpn ()
  (let ((cases '(("1 + 2" . (1 2 +))
                 ("1 + 2 + 3" . (1 2 + 3 +))
                 ("1 * 2 + 3" . (1 2 * 3 +))
                 ("1 + 2 * 3" . (1 2 3 * +))
                 ("1 + 2 + 3 * 4 * 5" . (1 2 + 3 4 * 5 * +)))))
    (dolist (case cases)
      (should (equal (basic-calculator-convert-to-rpn (car case)) (cdr case))))))

(ert-deftest basic-calculator-test ()
  (let ((cases '(("1 + 1" . 2)
                 (" 6-4 / 2 " . 4)
                 ("2*(5+5*2)/3+(6/2+8)" . 21)
                 ("(2+6* 3+5- (3*14/7+2)*5)+3" . -12))))
    (dolist (case cases)
      (should (equal (basic-calculator (car case)) (cdr case))))))