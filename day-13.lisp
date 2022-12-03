(defpackage :advent-of-code-2019/day-13
  (:use :cl)
  (:import-from :advent-of-code-2019/intcode)
  (:local-nicknames (:intcode :advent-of-code-2019/intcode)))

(in-package :advent-of-code-2019/day-13)

(defun run-arcade-cabinet (machine)
  (let* ((screen (make-hash-table :test 'equal))
         (output-state 0)
         (x 0)
         (y 0)
         (tile-id 0)
         (output-callback (lambda (instruction)
                            (case output-state
                              (0 (setf x instruction))
                              (1 (setf y instruction))
                              (2 (progn
                                   (setf tile-id instruction)
                                   (setf (gethash (cons x y) screen) tile-id))))
                            (setf output-state (mod (1+ output-state) 3)))))
    (intcode:run machine :output output-callback)
    (loop for value being the hash-values of screen counting (= 2 value))))
