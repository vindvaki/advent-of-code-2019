(defpackage :advent-of-code-2019/day-10
  (:use :cl)
  (:import-from #:alexandria
      #:hash-table-keys))

(in-package :advent-of-code-2019/day-10)

(defparameter *example* ".#..#
.....
#####
....#
...##")

(defun read-input (stream)
  (let* ((lines (loop for line = (read-line stream nil nil)
                      while line
                      collecting line))
         (rows (length lines))
         (cols (length (car lines))))
    (make-array (list rows cols) :initial-contents lines)))

(defun read-input-from-string (string)
  (with-input-from-string (stream string)
    (read-input stream)))

(defun asteroids-by-line-of-sight (matrix origin)
  (destructuring-bind (row0 col0) origin
    (let ((table (make-hash-table :test 'equal)))
      (when (char= #\# (aref matrix row0 col0))
        (destructuring-bind (rows cols) (array-dimensions matrix)
          (dotimes (row rows)
            (dotimes (col cols)
             (when (and (char= #\# (aref matrix row col))
                        (not (and (= row row0) (= col col0))))
               (let* ((dy (- row row0))
                      (dx (- col col0))
                      (gcd (gcd dx dy))
                      (key (list (/ dy gcd)
                                 (/ dx gcd)))
                      (value (list dy dx)))
                 (push value (gethash key table nil))))))))
      (maphash (lambda (key value)
                 (setf (gethash key table) (sort value #'<= :key #'squared-length)))
               table)
      table)))

(defun squared-length (list)
  (reduce (lambda (x y) (+ x (* y y))) list))

(defun load-input ()
  (with-open-file (stream "day-10.input")
    (read-input stream)))

(defun find-monitoring-station (input)
  (destructuring-bind (rows cols) (array-dimensions input)
    (let ((best-loc nil)
          (best-count -1))
      (dotimes (row rows)
        (dotimes (col cols)
          (let ((count (hash-table-count (asteroids-by-line-of-sight input (list row col)))))
            (when (> count best-count)
              (setf best-loc (list row col)
                    best-count count)))))
      (values best-loc best-count))))

(defun list+ (a b)
  (mapcar #'+ a b))

(defun part-1 (input)
  (find-monitoring-station input))

(defun part-2 (input &optional (count 200))
  (let* ((origin (find-monitoring-station input))
         (table (asteroids-by-line-of-sight input origin))
         (keys (sort (hash-table-keys table) #'<=
                     :key (lambda (key)
                            (destructuring-bind (y x) key
                              (if (= x 0)
                                  (* (signum y) (/ pi 2))
                                  (let ((atan (atan (/ y x))))
                                    (if (> x 0)
                                        (+ atan (/ pi 2))
                                        (+ atan (/ (* 3 pi) 2)))))))))
         (deleted 0))
    (loop while keys do
      (progn
        (dolist (key keys)
          (let* ((vec (pop (gethash key table)))
                 (point (list+ origin vec)))
            (destructuring-bind (row col) point
              (incf deleted)
              (when (= deleted count)
                 (return-from part-2
                   (values point
                           (+ (* 100 col) row)))))))
        (setf keys (delete-if-not (lambda (key) (gethash key table)) keys))))))
