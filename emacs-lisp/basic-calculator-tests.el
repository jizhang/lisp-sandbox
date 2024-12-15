(require 'ert)
(require 'basic-calculator)
(require 'test-utils)

(ert-deftest bc-test-reverse-polish-notation ()
  (let ((cases '(((2 1 + 3 *) . 9)
                 ((4 13 5 / +) . 6)
                 ((10 6 9 3 + -11 * / * 17 + 5 +) . 22)
                 ((1 u) . -1)
                 ((1 u 2 +) . 1)
                 ((1 2 u +) . -1)
                 ((1 2 + u) . -3)
                 ((1 u 2 u 3 u - u -) . 0)
                 ((2 3 ^) . 8)
                 ((2 2 3 ^ ^) . 256))))
    (assert-cases #'bc-reverse-polish-notation cases)))

(ert-deftest bc-test-tokenize ()
  (let ((cases '(("1 + 2" . (1 + 2))
                 ("-1" . (- 1))
                 ("-1 - -(-2 - -3)" . (- 1 - - \( - 2 - - 3 \)))
                 ("2 ^ 2 ^ 3" . (2 ^ 2 ^ 3)))))
    (assert-cases #'bc-tokenize cases)))

(ert-deftest bc-test-convert-to-rpn ()
  (let ((cases '(("1 + 2" . (1 2 +))
                 ("1 + 2 + 3" . (1 2 + 3 +))
                 ("1 * 2 + 3" . (1 2 * 3 +))
                 ("1 + 2 * 3" . (1 2 3 * +))
                 ("1 + 2 + 3 * 4 * 5" . (1 2 + 3 4 * 5 * +))
                 ("(1 + 2) * 3" . (1 2 + 3 *))
                 ("1 * (2 + 3)" . (1 2 3 + *))
                 ("1 + (2 + 3)" . (1 2 3 + +))
                 ("-1" . (1 u))
                 ("-(1 + 2)" . (1 2 + u))
                 ("-1 - -(-2 - -3)" . (1 u 2 u 3 u - u -))
                 ("-1 * -(2 + -3)" . (1 u 2 3 u + u *))
                 ("(1 + 2) * -3" . (1 2 + 3 u *))
                 ("1---1" . (1 1 u u -))
                 ("2 ^ 2 ^ 3" . (2 2 3 ^ ^)))))
    (assert-cases #'bc-convert-to-rpn cases)))

(ert-deftest bc-test-calculate ()
  (let ((cases '(("1 + 1" . 2)
                 (" 6-4 / 2 " . 4)
                 ("2*(5+5*2)/3+(6/2+8)" . 21)
                 ("(2+6* 3+5- (3*14/7+2)*5)+3" . -12)
                 ("-1 - -(-2 - -3)" . 0)
                 ("-(2+3)" . -5)
                 ("(-10+1)" . -9)
                 ("-(-(-1 + 2) * (3 + -4))" . -1)
                 ("2 ^ 2 ^ 3" . 256)
                 ("2*-(1+2)^-(2+5*-(2+4))" . -45753584909922)
                 ("3 + 4 * 2 - ( 1 - 5 ) ^ 2 ^ 3" . -65525))))
    (assert-cases #'bc-calculate cases)))
