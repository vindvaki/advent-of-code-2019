(defpackage :advent-of-code-2019/day-14
  (:use :cl)
  (:import-from :ppcre)
  (:import-from #:serapeum
                #:summing
                #:lines)
  (:import-from #:alexandria
                #:when-let
                #:define-constant)
  (:import-from #:uiop
                #:read-file-string))

(in-package :advent-of-code-2019/day-14)

(defparameter *example* "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

(define-constant +ore+ "ORE" :test 'equal)
(define-constant +fuel+ "FUEL" :test 'equal)

(defun parse-chemical-amount (string)
  (ppcre:register-groups-bind ((#'parse-integer amount) chemical)
      ("(\\d+) (\\w+)" string)
    (cons amount chemical)))

(defun parse-reaction (string)
  (destructuring-bind (inputs-string output-string)
      (ppcre:split " => " string)
    (let ((inputs (mapcar #'parse-chemical-amount (ppcre:split ", " inputs-string)))
          (output (parse-chemical-amount output-string)))
      (make-reaction
       :inputs inputs
       :output-amount (car output)
       :output-chemical (cdr output)))))

(defstruct reaction
  (inputs nil :type list)
  (output-amount nil :type integer)
  (output-chemical nil :type string))

(defun parse-reaction-table (string)
  (loop with table = (make-hash-table :test 'equal)
        for line in (lines string)
        for reaction = (parse-reaction line)
        for output-chemical = (reaction-output-chemical reaction) do
          (setf (gethash output-chemical table) reaction)
        finally (return table)))

(defun ore-cost (reaction-table &optional (chemical +fuel+) (amount 1) (have (make-hash-table :test 'equal)))
  (when-let ((reaction (gethash chemical reaction-table)))
    (let* ((output-amount (reaction-output-amount reaction))
           (times (ceiling amount output-amount)))
        (dolist (inputs (reaction-inputs reaction))
          (destructuring-bind (input-amount . input-chemical) inputs
            (let* ((input-have-amount (gethash input-chemical have 0))
                   (input-need-amount (max 0 (- (* times input-amount) input-have-amount))))
              (when (> input-need-amount 0)
                (ore-cost reaction-table input-chemical input-need-amount have))
              (decf (gethash input-chemical have 0) (* times input-amount)))))
        (incf (gethash chemical have 0) (* times output-amount))))
  (- (gethash +ore+ have 0)))

(defun load-input ()
  (read-file-string "day-14.input"))

(defun part-1 (input)
  (ore-cost (parse-reaction-table input)))

(defun part-2 (input &optional (total-ore 1000000000000))
  (do* ((table (parse-reaction-table input))
        (lo 1)
        (hi (loop for i = 1 then (* i 2)
                  until (> (ore-cost table +fuel+ i) total-ore)
                  finally (return i))))
       ((= hi (1+ lo)) lo)
    (let* ((mid (floor (+ hi lo) 2))
           (cost-mid (ore-cost table +fuel+ mid)))
      (if (>= cost-mid total-ore)
          (setf hi mid)
          (setf lo mid)))))
