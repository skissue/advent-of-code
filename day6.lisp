(defun parse-data (path)
  (let* ((lines (coerce (uiop:read-file-lines path) 'vector))
         (dimensions (cons (length (aref lines 0))
                           (length lines)))
         (obstacles)
         (guard))
    (loop for y below (cdr dimensions)
          do (loop for x below (car dimensions)
                   when (equal #\# (aref (aref lines y) x))
                     do (push (cons x y) obstacles)
                   when (equal #\^ (aref (aref lines y) x))
                     do (setf guard (cons x y))))
    (list obstacles guard dimensions)))

(defparameter *sample-data* (parse-data "sample/day6.txt"))
(defparameter *data* (parse-data "input/day6.txt"))

(defparameter *dir* '(0 . -1))
(defparameter *seen* nil)

(defun list-seen (data)
  (setf *dir* '(0 . -1))
  (setf *seen* nil)
  (destructuring-bind (obstacles guard dimensions) data
    (loop for (x . y) = guard
          for (dx . dy) = *dir*
          for nx = (+ x dx)
          for ny = (+ y dy)
          do (push (cons x y) *seen*)
          while (and (< -1 nx (car dimensions))
                     (< -1 ny (cdr dimensions)))
          when (member (cons nx ny) obstacles
                       :test #'equal)
            do (setf *dir* (cons (- dy) dx))
          else do (setf guard (cons nx ny)))
    (delete-duplicates *seen* :test #'equal)))

(defun part1 (data)
  (list-seen data)
  (length *seen*))

(defun probably-loops-p (data)
  (declare (optimize (speed 3) (safety 0)))
  (setf *dir* '(0 . -1))
  (destructuring-bind (obstacles guard dimensions) data
    (declare (type (cons fixnum fixnum) dimensions))
    (loop for i from 0
          for (x . y) of-type fixnum = guard
          for (dx . dy) of-type fixnum = *dir*
          for nx = (+ x dx)
          for ny = (+ y dy)
          when (> i 10000)
            return t
          unless (and (< -1 nx (car dimensions))
                      (< -1 ny (cdr dimensions)))
            return nil
          when (member (cons nx ny) obstacles
                       :test #'equal)
            do (setf *dir* (cons (- dy) dx))
          else do (setf guard (cons nx ny)))))

(defun part2 (data)
  (list-seen data)
  (destructuring-bind (obstacles guard dimensions) data
    (loop for (x . y) in *seen*
          when (and (not (or (member (cons x y) obstacles
                                     :test #'equal)
                             (equal (cons x y) guard)))
                    (probably-loops-p
                     (list (cons (cons x y) obstacles)
                           guard dimensions)))
            sum 1)))
