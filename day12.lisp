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

(defun part1 (data)
  (destructuring-bind (width height) (array-dimensions data)
    (loop with visited = nil
          for (x . y) = (cons 0 0) then (loop for x from 0
                                              when (loop for y from 0
                                                         unless (member (cons x y) visited :test #'equal)
                                                           return (cons x y))
                                                return it)
          sum)))

