(defpackage :advent-of-code-2019/day-13
  (:use :cl)
  (:import-from :advent-of-code-2019/intcode)
  (:import-from :bordeaux-threads)
  (:import-from #:alexandria
                #:define-constant
                #:copy-hash-table
                #:when-let)
  (:import-from #:serapeum
                #:drop
                #:take
                #:collecting)
  (:import-from #:sketch
                #:make-pen
                #:rect
                #:with-pen
                #:defsketch)
  (:local-nicknames (:intcode :advent-of-code-2019/intcode)
                    (:bt :bordeaux-threads)))

(in-package :advent-of-code-2019/day-13)

(defstruct (arcade-cabinet (:copier nil))
  (machine nil :type intcode:machine)
  (input nil :type list)
  (screen (make-hash-table :test 'equal) :type hash-table)
  (score 0 :type integer)
  (output-state 0 :type integer)
  (x 0 :type integer)
  (y 0 :type integer))

(defconstant +blank+ 0)
(defconstant +wall+ 1)
(defconstant +brick+ 2)
(defconstant +paddle+ 3)
(defconstant +ball+ 4)

(defun run-arcade-cabinet (arcade-cabinet &optional (on-checkpoint (lambda ())))
  (let* ((consumed nil)
         (output-callback (lambda (instruction &aux (is-checkpoint nil))
                            (with-slots (screen score output-state x y) arcade-cabinet
                              (case output-state
                                (0 (setf x instruction))
                                (1 (setf y instruction))
                                (2 (if (and (= x -1) (= y 0))
                                       (setf score instruction)
                                       (progn
                                         (if (= 0 instruction)
                                             (remhash (cons x y) screen)
                                             (setf (gethash (cons x y) screen) instruction))
                                         (when (and (= instruction +ball+)
                                                    (find +paddle+ (list (gethash (cons (1+ x) (1+ y)) screen 0)
                                                                         (gethash (cons (1- x) (1+ y)) screen 0)
                                                                         (gethash (cons x (1+ y)) screen 0))))
                                           (setf is-checkpoint t))))))
                              (prog1 (setf output-state (mod (1+ output-state) 3))
                                (when is-checkpoint
                                  (funcall on-checkpoint)))))))
    (intcode:run (arcade-cabinet-machine arcade-cabinet)
                 :output output-callback
                 :input (lambda () (let ((result (or (pop (arcade-cabinet-input arcade-cabinet)) 0)))
                                     (push result consumed)
                                     result)))
    arcade-cabinet))

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
    (arcade-cabinet-score (solve-arcade-cabinet arcade-cabinet (* 2 height)))))

(defun count-blocks (screen)
  (loop for value being the hash-values of screen counting (= 2 value)))

(defun solvedp (arcade-cabinet)
  (with-slots (screen) arcade-cabinet
    (and (> (hash-table-count screen) 0)
         (= 0 (count-blocks screen)))))

(defun copy-arcade-cabinet (arcade-cabinet &rest overrides)
  (with-slots (machine screen input score output-state x y) arcade-cabinet
    (let ((new-slots (list
                      :machine (intcode:copy-machine machine)
                      :screen (copy-hash-table screen)
                      :input input
                      :score score
                      :output-state output-state
                      :x x
                      :y y)))
      (loop for (key value) on overrides
            for i = 0 then (1+ i)
            when (evenp i) do
              (setf (getf new-slots key) value))
      (apply #'make-arcade-cabinet new-slots))))

(defun solve-arcade-cabinet (current max-moves &optional (visited (make-hash-table :test 'equal)))
  "Recursively play until done"
  (declare (optimize (speed 3)))
  (setf (gethash (arcade-cabinet-signature current) visited) t)
  (loop for direction in '(-1 +1) do
    (loop named inner
          for moves from 0 to max-moves
          for input = (make-list moves :initial-element direction)
          for next = (copy-arcade-cabinet current :input input)
          for snapshot = nil
          do (progn
               (run-arcade-cabinet next (lambda () (setf snapshot (copy-arcade-cabinet next))))
               (when snapshot
                 (unless (gethash (arcade-cabinet-signature snapshot) visited)
                   (when (> (arcade-cabinet-score next) 0)
                     ;; (make-instance 'intpong :screen (arcade-cabinet-screen next))
                     (return-from solve-arcade-cabinet next))
                   (let ((maybe-solved (solve-arcade-cabinet snapshot max-moves visited)))
                      (when maybe-solved
                          (return-from solve-arcade-cabinet maybe-solved)))))))))

(defun arcade-cabinet-signature (cabinet)
  (sort
   (loop for (x . y) being the hash-key of (arcade-cabinet-screen cabinet)
           using (hash-value z)
         collecting (list x y z))
   (lambda (a b)
     (destructuring-bind (ax ay az) a
       (destructuring-bind (bx by bz) b
         (or (< ax bx)
             (and (= ax bx)
                  (<= ay by))
             (and (= ax bx)
                  (= ay by)
                  (<= az bz))))))))

(defsketch intpong
    ((name "intpong")
     (screen nil)
     (pixel-scale 10))
  (loop for (x . y) being the hash-key of screen using (hash-value tile-id) do
    (let ((color (ecase tile-id
                   (0 sketch:+white+)
                   (1 sketch:+black+)
                   (2 sketch:+yellow+)
                   (3 sketch:+red+)
                   (4 sketch:+green+))))
      (with-pen (make-pen :fill color)
        (rect (* pixel-scale x)
              (* pixel-scale y)
              pixel-scale
              pixel-scale)))))
