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

(defparameter *seen* (make-hash-table :test #'equal))
(defun expand-shape (data point &optional value)
  (destructuring-bind ((x . y) (width height))
      (list point (array-dimensions data))
    (unless value
      (setf value (aref data x y)))
    (setf (gethash (cons x y) *seen*) t)
    (loop for (dx . dy) in +dirs+
          for nx = (+ x dx)
          for ny = (+ y dy)
          when (and (< -1 nx width)
                    (< -1 ny height)
                    (equal value (aref data nx ny))
                    (not (gethash (cons nx ny) *seen*)))
            do (expand-shape data (cons nx ny) value))))

(defun perimeter (points)
  (loop for (x . y) being the hash-keys of points
        sum (loop for (dx . dy) in +dirs+
                  for nx = (+ x dx)
                  for ny = (+ y dy)
                  count (not (gethash (cons nx ny) points)))))

(defun part1 (data)
  (loop with (width height) = (array-dimensions data)
        with visited = (make-hash-table :test #'equal)
        for x below width
        sum (loop for y below height
                  when (not (gethash (cons x y) visited))
                    do (setf *seen* (make-hash-table :test #'equal))
                       (expand-shape data (cons x y))
                       (loop for key being the hash-keys of *seen*
                             do (setf (gethash key visited) t))
                    and sum (* (hash-table-count *seen*)
                               (perimeter *seen*)))))

