(defpackage :advent-of-code-2019/day-12
  (:use :cl)
  (:import-from :ppcre)
  (:import-from #:serapeum
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:alexandria
                #:if-let
                #:with-gensyms))

(in-package :advent-of-code-2019/day-12)


(defstruct moon
  (pos (list 0 0 0) :type list)
  (vel (list 0 0 0) :type list))

(defun parse-moon (string)
  (ppcre:register-groups-bind
      ((#'parse-integer x) (#'parse-integer y) (#'parse-integer z))
      ("<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>" string)
    (make-moon :pos (list x y z))))

(defun parse-input (input)
  (mapcar #'parse-moon (lines input)))

(defun load-input ()
  (parse-input (read-file-string "day-12.input")))

(defmacro do-pairs ((first second list &optional result) &body body)
  (with-gensyms (eval-list rest)
    `(do* ((,eval-list ,list)
           (,first (car ,eval-list) (car ,rest))
           (,rest (cdr ,eval-list) (cdr ,rest)))
          ((not ,rest) ,result)
       (dolist (,second ,rest)
         ,@body))))

(defun apply-gravity (moons)
  (do-pairs (first second moons moons)
    (do* ((first-pos (moon-pos first) (cdr first-pos))
          (first-vel (moon-vel first) (cdr first-vel))
          (second-pos (moon-pos second) (cdr second-pos))
          (second-vel (moon-vel second) (cdr second-vel)))
        ((not first-pos))
      (let ((signum (signum (- (car first-pos) (car second-pos)))))
        (decf (car first-vel) signum)
        (incf (car second-vel) signum)))))

(defun apply-velocity (moons)
  (dolist (moon moons moons)
    (setf (moon-pos moon) (mapcar #'+ (moon-pos moon) (moon-vel moon)))))

(defun simulate-once (moons)
  (apply-gravity moons)
  (apply-velocity moons))

(defun potential-energy (moon)
  (loop for x in (moon-pos moon)
        summing (abs x)))

(defun kinetic-energy (moon)
  (loop for x in (moon-vel moon)
        summing (abs x)))

(defmethod total-energy ((obj moon))
  (* (potential-energy obj)
     (kinetic-energy obj)))

(defmethod total-energy ((obj list))
  (loop for moon in obj
        summing (total-energy moon)))

(defun part-1 (moons &optional (count 1000))
  (dotimes (i count)
    (simulate-once moons))
  (total-energy moons))

(defun find-coordinate-cycles (moons)
  (loop with seen = (make-hash-table :test 'equal)
        with cycles = (make-hash-table :test 'equal)
        for iteration fixnum = 0 then (1+ iteration)
        until (= (hash-table-count cycles) 3)
        do (progn
             (dotimes (i 3)
               (let ((key (mapcar (lambda (moon) (cons (nth i (moon-pos moon))
                                                       (nth i (moon-vel moon))))
                                  moons)))
                 (if-let ((offset (gethash key seen)))
                   (unless (gethash i cycles)
                     (setf (gethash i cycles) (list iteration offset)))
                   (setf (gethash key seen) iteration))))
             (simulate-once moons))
        finally (return cycles)))

(defun find-cycle (moons)
  (apply #'lcm
    (loop for (iteration offset) being the hash-values of (find-coordinate-cycles moons)
          collecting (- iteration offset))))

(defun part-2 (moons)
  (find-cycle moons))

(defparameter *simple-example* "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>")

(defparameter *complex-example* "<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>")
