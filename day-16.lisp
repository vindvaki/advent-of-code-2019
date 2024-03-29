(defpackage :advent-of-code-2019/day-16
  (:use :cl)
  (:import-from #:alexandria
                #:ensure-gethash
                #:if-let
                #:define-constant)
  (:import-from #:serapeum
                #:drop
                #:->
                #:trim-whitespace
                #:take)
  (:import-from #:uiop
                #:read-file-string))

(in-package :advent-of-code-2019/day-16)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vector-equal (xs ys)
    (and (equal (array-dimensions xs)
                (array-dimensions ys))
         (loop for x across xs
               for y across ys
               always (= x y)))))

(define-constant +pattern+ #(0 1 0 -1) :test #'vector-equal)
(define-constant +pattern-length+ (length +pattern+))

(defun digits (number &optional (base 10))
  (reverse
    (loop
      for n = number then (floor n 10)
      for d = (mod n base)
      while (> n 0)
      collecting (mod n base))))

(-> flawed-frequency-transmission (list &optional boolean) list)
(defun flawed-frequency-transmission (sig &optional (debug nil))
  (declare (optimize (speed 3)))
  (loop with n = (length sig)
        for i fixnum from 1 to n
        for d fixnum = (loop for x fixnum in sig
                             for j fixnum = 1 then (1+ j)
                             for y fixnum = (aref +pattern+ (mod (floor j i) +pattern-length+))
                             when debug
                               do (let ((s (if (= (1+ j) n) "=" "+")))
                                    (format t "~D*~D ~A " x y s))
                             summing (the fixnum (* x y)))
        when debug
          do (format t "~D ~%" d)
        collecting (abs (rem d 10))))

(defun part-1 (input)
  (let ((sig (parse-input input)))
    (dotimes (i 100)
      (setf sig (flawed-frequency-transmission sig)))
    (number-from-digits (take 8 sig))))

;; For part 2, observe that
;;
;; 1. for _our_ input, the offset happens to be well beyond n/2, where n = (length input)
;; 2. from n/2 onwards, digit i is just the sum of the last n-i digits in the signal
(defun part-2 (input)
  (let* ((base (parse-input input))
         (sig (loop for i from 1 to 10000 appending base))
         (offset (number-from-digits (take 7 sig)))
         (rest (reverse (drop offset sig))))
    (dotimes (i 100)
      (setf rest (mapcar (lambda (x) (mod x 10)) (cumsum rest))))
    (number-from-digits (take 8 (reverse rest)))))

(defun cumsum (list)
  (loop for x in list
        for y = x then (+ y x)
        collecting y))

(defun parse-input (input)
 (loop for c across input
       for d = (- (char-code c) (char-code #\0))
       collecting d))

(defun number-from-digits (digits &optional (base 10))
  (loop for d in digits
        for n = d then (+ d (* base n))
        finally (return n)))

(defun load-input ()
  (trim-whitespace (read-file-string "day-16.input")))
