(defpackage :advent-of-code-2019/intcode
  (:use :cl)
  (:import-from #:serapeum
                #:->
                #:split-sequence)
  (:import-from #:uiop
                #:read-file-string))

(in-package :advent-of-code-2019/intcode)

(defparameter *example* (machine-from-string "1,9,10,3,2,3,11,0,99,30,40,50"))

(defstruct machine
  (cursor 0 :type fixnum)
  (data (make-array 0) :type simple-vector)
  (in nil :type list)
  (out nil :type list))

(defun machine-from-list (list)
  (make-machine :data (make-array (length list) :initial-contents list)))

(defun machine-from-string (string)
  (let ((data-list (mapcar #'parse-integer (split-sequence #\, string))))
    (machine-from-list data-list)))

(defun machine-from-file (pathname)
  (machine-from-string (read-file-string pathname)))

(defun mref (machine position)
  (svref (machine-data machine) position))

(defun shift (machine)
  (prog1 (mref machine (machine-cursor machine))
    (incf (machine-cursor machine))))

(defun read-input (machine)
  (pop (machine-in machine)))

(defun store (machine position value)
  (setf (svref (machine-data machine) position) value))

(defun output (machine value)
  (push value (machine-out machine)))

(defun next (machine)
  "Executes one round of `machine', modifying machine and returning it again for the
next round. Returns `nil' if execution is done."
  (values
   machine
   (let ((instruction (shift machine)))
     (multiple-value-bind (opcode next-param) (split-instruction machine instruction)
       (ecase opcode
         (99 nil)
         (1 (op-store-binop machine #'+ next-param))
         (2 (op-store-binop machine #'* next-param))
         (3 (let ((value (read-input machine))
                  (param (shift machine)))
              (store machine param value)))
         (4 (output machine (funcall next-param)))
         (5 (op-jump-if-true machine next-param))
         (6 (op-jump-if-false machine next-param))
         (7 (op-less-than machine next-param))
         (8 (op-equals machine next-param)))))))

(-> op-store-binop (machine function (function () fixnum)) t)
(defun op-store-binop (machine binop next-param)
  (let ((in1 (funcall next-param))
        (in2 (funcall next-param))
        (out (shift machine))) ;; output params are always in position mode
    (store machine out (funcall binop in1 in2))))

(-> op-jump-if-true (machine (function () fixnum)) t)
(defun op-jump-if-true (machine next-param)
  (if (/= 0 (funcall next-param))
    (setf (machine-cursor machine) (funcall next-param))
    (incf (machine-cursor machine))))

(-> op-jump-if-false (machine (function() fixnum)) t)
(defun op-jump-if-false (machine next-param)
  (if (= 0 (funcall next-param))
    (setf (machine-cursor machine) (funcall next-param))
    (incf (machine-cursor machine))))

(-> op-less-than (machine (function () fixnum)) t)
(defun op-less-than (machine next-param)
  (if (< (funcall next-param) (funcall next-param))
    (store machine (shift machine) 1)
    (store machine (shift machine) 0)))

(-> op-equals (machine (function () fixnum)) t)
(defun op-equals (machine next-param)
  (if (= (funcall next-param) (funcall next-param))
    (store machine (shift machine) 1)
    (store machine (shift machine) 0)))

(-> split-instruction (machine fixnum) (values fixnum function))
(defun split-instruction (machine instruction)
  "Splits an instruction into the opcode and the parameter modes"
  (multiple-value-bind (param-modes opcode) (floor instruction 100)
    (values opcode (next-param machine param-modes))))

(defun next-param (machine param-modes)
  (let ((next-mode (next-digit param-modes)))
    (lambda ()
      (let ((value (shift machine)))
        (ecase (funcall next-mode)
          (0 (mref machine value))
          (1 value))))))

(-> next-digit (fixnum) function)
(defun next-digit (number)
  (let ((iter number))
    (declare (fixnum iter))
    (lambda ()
      (multiple-value-bind (next-iter digit) (floor iter 10)
        (setf iter next-iter)
        digit))))

(defun run (machine &key (input nil))
  (setf (machine-in machine) input)
  (multiple-value-bind (next-machine has-next) (next machine)
    (if has-next
        (run next-machine)
        (values machine (svref (machine-data machine) 0)))))
