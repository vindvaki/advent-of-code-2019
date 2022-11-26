(defpackage :advent-of-code-2019/day-11
  (:use :cl)
  (:import-from :advent-of-code-2019/intcode)
  (:import-from #:trivia
                #:ematch)
  (:local-nicknames (:intcode :advent-of-code-2019/intcode)))

(in-package :advent-of-code-2019/day-11)

(defconstant +black+ 0)
(defconstant +white+ 1)

(defun run-painter-robot (machine &optional (seed 0))
  (let* ((panels (make-hash-table :test 'equal))
         (current-panel (cons 0 0))
         (current-direction (cons 1 0))
         (input-callback (lambda ()
                           (gethash current-panel panels +black+)))
         (output-state 0)
         (output-callback (lambda (value)
                            (case output-state
                              (0 (setf (gethash (copy-list current-panel) panels) value))
                              (1 (progn
                                   (setf current-direction (next-direction current-direction value))
                                   (incf (car current-panel) (car current-direction))
                                   (incf (cdr current-panel) (cdr current-direction)))))
                            (setf output-state (mod (1+ output-state) 2)))))
    (setf (gethash (copy-list current-panel) panels) seed)
    (intcode:run machine
                 :input input-callback
                 :output output-callback)
    panels))

(defun next-direction (current-direction instruction)
  (ecase instruction
    (0
     (ematch current-direction
       ((cons 1 0) (cons 0 -1))
       ((cons 0 -1) (cons -1 0))
       ((cons -1 0) (cons 0 1))
       ((cons 0 1) (cons 1 0))))
    (1
     (ematch current-direction
       ((cons 1 0) (cons 0 1))
       ((cons 0 1) (cons -1 0))
       ((cons -1 0) (cons 0 -1))
       ((cons 0 -1) (cons 1 0))))))

(defun load-input ()
  (uiop:read-file-string "day-11.input"))

(defun part-1 (input)
  (hash-table-count (run-painter-robot (intcode:machine-from-string input))))

(defun print-panels (panels)
  (let ((min-row 0)
        (max-row 0)
        (min-col 0)
        (max-col 0))
    ;; calculate bounding box
    (maphash (lambda (key value)
               (declare (ignore value))
               (destructuring-bind (row . col) key
                 (setf min-row (min min-row row)
                       max-row (max max-row row)
                       min-col (min min-col col)
                       max-col (max max-col col))))
             panels)
    (loop for row from max-row downto min-row do
      (loop for col from min-col to max-col do
        (write-char (if (= +white+ (gethash (cons row col) panels +black+))
                        #\#
                        #\.))
        finally (write-char #\Newline)))))
