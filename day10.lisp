(defun parse-data (path)
  (let* ((lines (uiop:read-file-lines path))
         (width (length (car lines)))
         (height (length lines))
         (grid (make-array (list width height))))                            
    (loop for y from 0
          for l in lines
          do (loop for x below width
                   do (setf (aref grid x y)
                            (parse-integer
                             (string (aref l x))))))
    grid))

(defparameter *sample-data* (parse-data "sample/day10.txt"))
(defparameter *data* (parse-data "input/day10.txt"))

(defconstant +dirs+ '((1 . 0)
                      (0 . 1)
                      (-1 . 0)
                      (0 . -1)))

(defun append-if-=-in-dir (data x y dir lookup target points)
  (destructuring-bind (dx . dy) dir
    (let ((nx (+ x dx))
          (ny (+ y dy)))
      (handler-case
          (when (= target (aref lookup nx ny))
            (setf (aref data nx ny)
                  (append (aref data nx ny) points)))
        (sb-int:invalid-array-index-error ())))))

(defun reachability-grid (data)
  (destructuring-bind (width height) (array-dimensions data)
    (let ((reach-grid (make-array (list width height) :initial-element nil)))
      (loop for x below width
            do (loop for y below height
                     when (= (aref data x y) 9)
                       do (push (cons x y) (aref reach-grid x y))))
      (loop for h from 9 downto 1
            do (loop for x below width
                     do (loop for y below height
                              do (setf (aref reach-grid x y)
                                       (delete-duplicates (aref reach-grid x y)
                                                          :test #'equal))
                              when (= (aref data x y) h)
                                do (loop for dir in +dirs+
                                         do (append-if-=-in-dir
                                             reach-grid x y dir data (1- h) (aref reach-grid x y))))))
      reach-grid)))

(defun part1 (data)
  (destructuring-bind (width height) (array-dimensions data)
    (let ((reach-grid (reachability-grid data)))
      (loop for x below width
            sum (loop for y below height
                      when (= (aref data x y) 0)
                        sum (length (delete-duplicates (aref reach-grid x y))))))))

