(defpackage :advent-of-code-2019/day-13
  (:use :cl)
  (:import-from :advent-of-code-2019/intcode)
  (:import-from :bordeaux-threads)
  (:import-from #:alexandria
                #:copy-hash-table
                #:when-let)
  (:import-from #:serapeum
                #:collecting)
  (:local-nicknames (:intcode :advent-of-code-2019/intcode)
                    (:bt :bordeaux-threads)))

(in-package :advent-of-code-2019/day-13)

(defstruct (arcade-cabinet (:copier nil))
  (machine nil :type intcode:machine)
  (screen (make-hash-table :test 'equal) :type hash-table)
  (input nil :type list)
  (score 0 :type integer))

(defun run-arcade-cabinet (arcade-cabinet &optional (on-score-change #'identity))
  (let* ((output-state 0)
         (x 0)
         (y 0)
         (tile-id 0)
         (output-callback (lambda (instruction)
                            (case output-state
                              (0 (setf x instruction))
                              (1 (setf y instruction))
                              (2 (progn
                                   (setf tile-id instruction)
                                   (if (>= x 0)
                                       (setf (gethash (cons x y) (arcade-cabinet-screen arcade-cabinet)) tile-id)
                                       (when (> tile-id 0)
                                         (incf (arcade-cabinet-score arcade-cabinet) tile-id)
                                         (funcall on-score-change arcade-cabinet))))))
                            (setf output-state (mod (1+ output-state) 3)))))
    (intcode:run (arcade-cabinet-machine arcade-cabinet)
                 :output output-callback
                 :input (joystick-input-list-reader (arcade-cabinet-input arcade-cabinet)))
    arcade-cabinet))

(defun joystick-input-list-reader (list)
  (lambda ()
    (or (pop list) 0)))

(defun load-input ()
  (uiop:read-file-string "day-13.input"))

(defun part-1 ()
  (let* ((input (load-input))
         (machine (intcode:machine-from-string input))
         (arcade-cabinet (make-arcade-cabinet :machine machine)))
    (run-arcade-cabinet arcade-cabinet)
    (count-blocks (arcade-cabinet-screen arcade-cabinet))))

(defun part-2 ()
  (let* ((input (load-input))
         (machine (intcode:machine-from-string input))
         (arcade-cabinet (make-arcade-cabinet :machine machine))
         (first-run (run-arcade-cabinet (copy-arcade-cabinet arcade-cabinet)))
         (height (loop for (x . y) being the hash-keys of (arcade-cabinet-screen first-run)
                       maximizing y)))
    (intcode::store (arcade-cabinet-machine arcade-cabinet) 0 2)
    (solve-arcade-cabinet arcade-cabinet (- (* 2 height)) (* 2 height))))

(defun count-blocks (screen)
  (loop for value being the hash-values of screen counting (= 2 value)))

(defun solvedp (arcade-cabinet)
  (= 0 (count-blocks (arcade-cabinet-screen arcade-cabinet))))

(defun copy-arcade-cabinet (arcade-cabinet)
  (with-slots (machine screen input score) arcade-cabinet
    (make-arcade-cabinet
     :machine (intcode:copy-machine machine)
     :screen (copy-hash-table screen)
     :input input
     :score score)))

(defun make-joystick-input (signed-amount)
  (let ((signum (signum signed-amount)))
    (collecting
      (dotimes (i (abs signed-amount))
        (collect signum)))))

(defun solve-arcade-cabinet (current min-input max-input)
  "Recursively play until done"
  (loop for input from min-input to max-input do
    (let* ((next (copy-arcade-cabinet current))
           (snapshot nil))
      (setf (arcade-cabinet-input next) (make-joystick-input input))
      (run-arcade-cabinet next (lambda (_)
                                 (declare (ignore _))
                                 (setf snapshot (copy-arcade-cabinet current))))
      (when snapshot
        (solve-arcade-cabinet snapshot min-input max-input)
        (when (solvedp snapshot)
          (return-from solve-arcade-cabinet snapshot))))))
