(defpackage :advent-of-code-2019-tests/tests/all
  (:use :cl :fiveam)
  (:local-nicknames (:intcode :advent-of-code-2019/intcode))
  (:local-nicknames (:day-10 :advent-of-code-2019/day-10))
  (:local-nicknames (:day-11 :advent-of-code-2019/day-11)))

(in-package :advent-of-code-2019-tests/tests/all)

(def-suite* all) ; entrypoint test suite

(test regression-day-9
  (is (= 3780860499 (intcode:call (intcode:machine-from-file "day-9.input") 1)))
  (is (= 33343 (intcode:call (intcode:machine-from-file "day-9.input") 2))))

(test regression-day-10
  (is (= 221 (nth-value 1 (day-10:part-1 (day-10:load-input)))))
  (is (= 806 (nth-value 1 (day-10:part-2 (day-10:load-input))))))

(test regression-day-11
  (is (= 1930 (day-11:part-1 (day-11:load-input)))))
