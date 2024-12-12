(defun parse-data (path)
  (let* ((lines (uiop:read-file-lines path))
         (width (length (car lines)))
         (height (length lines))
         (grid (make-array (list width height))))                            
    (loop for y from 0
          for l in lines
          do (loop for x below width
                   do (setf (aref grid x y) (string (aref l x)))))
    grid))

(defparameter *sample-data* (parse-data "sample/day12.txt"))
(defparameter *data* (parse-data "input/day12.txt"))

(defconstant +dirs+ '((0 . 1)
                      (1 . 0)
                      (-1 . 0)
                      (0 . -1)))

(defparameter *seen* nil)
(defun expand-shape (data point &optional value)
  (destructuring-bind ((x . y) (width height))
      (list point (array-dimensions data))
    (unless value
      (setf value (aref data x y)))
    (push (cons x y) *seen*)
    (loop for (dx . dy) in +dirs+
          for nx = (+ x dx)
          for ny = (+ y dy)
          when (and (< -1 nx width)
                    (< -1 ny height)
                    (equal value (aref data nx ny))
                    (not (member (cons nx ny) *seen* :test #'equal)))
            do (expand-shape data (cons nx ny) value))))

(defun perimeter (points)
  (loop for (x . y) in points
        sum (loop for (dx . dy) in +dirs+
                  for nx = (+ x dx)
                  for ny = (+ y dy)
                  count (not (member (cons nx ny) points :test #'equal)))))

(defun part1 (data)
  (loop with (width height) = (array-dimensions data)
        with visited = nil
        for x below width
        sum (loop for y below height
                  when (not (member (cons x y) visited :test #'equal))
                    do (setf *seen* nil)
                       (expand-shape data (cons x y))
                       (setf visited (append visited *seen*))
                    and sum (* (length *seen*)
                               (perimeter *seen*)))))

