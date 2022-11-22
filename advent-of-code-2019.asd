(defsystem :advent-of-code-2019
  :class :package-inferred-system
  :name "Hordur's solutions to Advent of Code 2019"
  :author "Hordur Freyr Yngvason"
  :license "MIT"
  :depends-on ("advent-of-code-2019/solutions")
  :in-order-to ((test-op (test-op :advent-of-code-2019-tests))))

(register-system-packages "cl-ppcre" '(:ppcre))
