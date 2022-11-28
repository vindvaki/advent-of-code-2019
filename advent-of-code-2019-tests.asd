(defsystem :advent-of-code-2019-tests
  :class :package-inferred-system
  :author "Hordur Freyr Yngvason"
  :license "MIT"
  :depends-on ("advent-of-code-2019-tests/tests/all")
  :perform (test-op (op c)
                    (symbol-call :advent-of-code-2019-tests/tests/all :run!
                                 (find-symbol* :all :advent-of-code-2019-tests/tests/all))))
