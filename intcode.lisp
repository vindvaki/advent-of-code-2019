(defpackage :advent-of-code-2019/intcode
  (:use :cl)
  (:import-from #:serapeum
                #:->
                #:split-sequence)
  (:import-from #:uiop
                #:read-file-string))

(in-package :advent-of-code-2019/intcode)

;; (defparameter *example* (machine-from-string "1,9,10,3,2,3,11,0,99,30,40,50"))

(defstruct machine
  (cursor 0 :type integer)
  (data (make-hash-table) :type hash-table)
  (in nil :type list)
  (out nil :type list)
  (relative-base 0 :type integer))

(defun machine-from-list (list)
  (make-machine :data (loop with table = (make-hash-table)
                            for value in list
                            for key fixnum = 0 then (1+ key) do
                              (setf (gethash key table) value)
                            finally (return table))))

(defun machine-from-string (string)
  (let ((data-list (mapcar #'parse-integer (split-sequence #\, string))))
    (machine-from-list data-list)))

(defun machine-from-file (pathname)
  (machine-from-string (read-file-string pathname)))

(-> mref (machine integer) integer)
(defun mref (machine position)
  (when (< position 0)
    (error "cannot read from negative position"))
  (gethash position (machine-data machine) 0))

(-> relpos (machine integer integer) integer)
(defun relpos (machine position mode)
  (ecase mode
    (0 position)
    (2 (+ position (machine-relative-base machine)))))

(defun shift (machine)
  (prog1 (mref machine (machine-cursor machine))
    (incf (machine-cursor machine))))

(defun read-input (machine)
  (or
   (pop (machine-in machine))
   (error "attempted read from empty input")))

(defun store (machine position value)
  (when (< position 0)
    (error "cannot write to negative position"))
  (setf (gethash position (machine-data machine))
        value))

(defun output (machine value)
  (push value (machine-out machine)))

(defun store-next-param (machine next-param value)
  (multiple-value-bind (old-position position mode) (funcall next-param)
    (declare (ignore old-position))
    (store machine (relpos machine position mode) value)))

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
         (3 (let ((value (read-input machine)))
              (store-next-param machine next-param value)))
         (4 (output machine (funcall next-param)))
         (5 (op-jump-if-true machine next-param))
         (6 (op-jump-if-false machine next-param))
         (7 (op-less-than machine next-param))
         (8 (op-equals machine next-param))
         (9 (let ((param (funcall next-param)))
              (incf (machine-relative-base machine) param))))))))

(-> op-store-binop (machine function (function () integer)) t)
(defun op-store-binop (machine binop next-param)
  (let ((in1 (funcall next-param))
        (in2 (funcall next-param)))
    (store-next-param machine next-param (funcall binop in1 in2))))

(-> op-jump-if-true (machine (function () integer)) t)
(defun op-jump-if-true (machine next-param)
  (if (/= 0 (funcall next-param))
      (setf (machine-cursor machine) (funcall next-param))
      (incf (machine-cursor machine))))

(-> op-jump-if-false (machine (function() integer)) t)
(defun op-jump-if-false (machine next-param)
  (if (= 0 (funcall next-param))
      (setf (machine-cursor machine) (funcall next-param))
      (incf (machine-cursor machine))))

(-> op-less-than (machine (function () integer)) t)
(defun op-less-than (machine next-param)
  (if (< (funcall next-param) (funcall next-param))
      (store-next-param machine next-param 1)
      (store-next-param machine next-param 0)))

(-> op-equals (machine (function () integer)) t)
(defun op-equals (machine next-param)
  (if (= (funcall next-param) (funcall next-param))
      (store-next-param machine next-param 1)
      (store-next-param machine next-param 0)))

(-> split-instruction (machine integer) (values integer function))
(defun split-instruction (machine instruction)
  "Splits an instruction into the opcode and the parameter modes"
  (multiple-value-bind (param-modes opcode) (floor instruction 100)
    (values opcode (next-param machine param-modes))))

(-> next-param (machine integer) (function () integer))
(defun next-param (machine param-modes)
  (let ((next-mode (next-digit param-modes)))
    (lambda ()
      (let ((value (shift machine))
            (mode (funcall next-mode)))
        (values
         (ecase mode
           (0 (mref machine value))
           (1 value)
           (2 (mref machine (relpos machine value mode))))
         value
         mode)))))

(-> next-digit (integer) function)
(defun next-digit (number)
  (let ((iter number))
    (declare (integer iter))
    (lambda ()
      (multiple-value-bind (next-iter digit) (floor iter 10)
        (setf iter next-iter)
        digit))))

(defun run (machine &key (input nil))
  (setf (machine-in machine) input)
  (loop named runner do
    (multiple-value-bind (next-machine has-next) (next machine)
      (if has-next
          (setf machine next-machine)
          (return-from runner))))
  (values machine (mref machine 0)))
