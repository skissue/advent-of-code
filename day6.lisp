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

(defun part1 (data)
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
    (length (delete-duplicates *seen* :test #'equal))))


