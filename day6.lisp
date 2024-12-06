(ql:quickload :lparallel)

(setf lparallel:*kernel* (lparallel:make-kernel 16))

(defun parse-data (path)
  (let* ((lines (coerce (uiop:read-file-lines path) 'vector))
         (dimensions (cons (length (aref lines 0))
                           (length lines)))
         (obstacles (make-array (list (car dimensions)
                                      (cdr dimensions))
                                :initial-element nil))
         (guard))
    (loop for y below (cdr dimensions)
          do (loop for x below (car dimensions)
                   when (equal #\# (aref (aref lines y) x))
                     do (setf (aref obstacles x y) t)
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
          when (aref obstacles nx ny)
            do (setf *dir* (cons (- dy) dx))
          else do (setf guard (cons nx ny)))
    (setf *seen* (delete-duplicates *seen* :test #'equal))))

(defun part1 (data)
  (list-seen data)
  (length *seen*))

(defun probably-loops-p (data)
  (declare (optimize (speed 3) (safety 0)))
  (let ((dir '(0 . -1)))
    (destructuring-bind (obstacles guard dimensions) data
      (declare (type (cons fixnum fixnum) dimensions)
               (type (simple-array boolean (* *)) obstacles))
      (loop for i from 0
            for (x . y) of-type fixnum = guard
            for (dx . dy) of-type fixnum = dir
            for nx = (+ x dx)
            for ny = (+ y dy)
            when (> i 10000)
              return t
            unless (and (< -1 nx (car dimensions))
                        (< -1 ny (cdr dimensions)))
              return nil
            when (aref obstacles nx ny)
              do (setf dir (cons (- dy) dx))
            else do (setf guard (cons nx ny))))))

(defun part2 (data)
  (list-seen data)
  (destructuring-bind (obstacles guard dimensions) data
    (lparallel:pcount-if
     (lambda (a)
       (destructuring-bind (x . y) a
         (and (not (or (aref obstacles x y)
                       (equal (cons x y) guard)))
              (probably-loops-p
               (list (let ((c (alexandria:copy-array obstacles)))
                       (setf (aref c x y) t)
                       c)
                     guard dimensions)))))
     *seen*)))
