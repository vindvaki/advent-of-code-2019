(defpackage :advent-of-code-2019/day-15
  (:use :cl)
  (:import-from #:serapeum)
  (:import-from #:uiop
                #:read-file-string)
  (:local-nicknames (:intcode :advent-of-code-2019/intcode)))

(in-package :advent-of-code-2019/day-15)

(defconstant +north+ 1)
(defconstant +south+ 2)
(defconstant +west+ 3)
(defconstant +east+ 4)

(defun direction-list (n)
  (serapeum:select n
    (+north+ '(-1 0))
    (+south+ '(+1 0))
    (+west+ '(0 -1))
    (+east+ '(0 +1))))

(defun apply-direction (coordinate direction)
  (mapcar #'+ coordinate (direction-list direction)))

(defun load-machine ()
  (intcode:machine-from-string (read-file-string "day-15.input")))

(defun find-oxygen-system (machine)
  (declare (optimize (speed 3)))
  (do ((visited (make-hash-table :test 'equal))
       (queue (serapeum:queue (list (list 0 0) nil))))
      ((serapeum:queue-empty-p queue))
    (destructuring-bind (coordinate steps) (serapeum:deq queue)
      (loop for next-machine = (intcode:copy-machine machine)
            for input from 1 to 4
            for output = nil
            for next-coordinate = (apply-direction coordinate input)
            for next-steps = (append steps (list input))
            unless (gethash next-coordinate visited)
              do (progn
                   (setf (gethash coordinate visited) t)
                   (intcode:run next-machine
                               :output (lambda (out) (setf output out))
                               :input (intcode:list-input-reader (append next-steps (list 99))))
                   (ecase output
                     (0 nil)
                     (1 (serapeum:enq (list next-coordinate next-steps) queue))
                     (2 (return-from find-oxygen-system (values steps coordinate)))))))))

(defun part-1 ()
  (1+ (length (find-oxygen-system (load-machine)))))

(defun bfs-fill (machine source initial-steps &aux (max-distance 0))
  (declare (optimize (speed 3)))
  (do ((visited (make-hash-table :test 'equal))
       (queue (serapeum:queue (list source initial-steps 0))))
      ((serapeum:queue-empty-p queue))
    (destructuring-bind (coordinate steps distance) (serapeum:deq queue)
      (loop for next-machine = (intcode:copy-machine machine)
            for input from 1 to 4
            for output = nil
            for next-coordinate = (apply-direction coordinate input)
            for next-steps = (append steps (list input))
            unless (gethash next-coordinate visited)
              do (progn
                   (setf (gethash coordinate visited) t
                         max-distance distance)
                   (intcode:run next-machine
                               :output (lambda (out) (setf output out))
                               :input (intcode:list-input-reader (append next-steps (list 99))))
                   (ecase output
                     (0 nil)
                     (1 (serapeum:enq (list next-coordinate next-steps (1+ max-distance)) queue))
                     (2 nil))))))
  (1+ max-distance))

(defun part-2 ()
  (let ((machine (load-machine)))
    (multiple-value-bind (steps source) (find-oxygen-system machine)
      (bfs-fill machine source steps))))
