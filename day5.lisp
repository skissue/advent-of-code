(ql:quickload :cl-ppcre)

(defun parse-data (path)
  (let* ((lines (uiop:read-file-lines path))
         (pivot (position "" lines :test #'equal))
         (orders (mapcar (lambda (x)
                           (mapcar #'parse-integer
                                   (cl-ppcre:split "\\|" x)))
                         (subseq lines 0 pivot)))
         (updates (mapcar (lambda (x)
                            (mapcar #'parse-integer
                                    (cl-ppcre:split "," x)))
                          (subseq lines (1+ pivot)))))
    (cons orders updates)))

(defparameter *sample-data* (parse-data "sample/day5.txt"))
(defparameter *data* (parse-data "input/day5.txt"))

(defparameter *rules* nil)

(defun fill-rules (orders)
  (setf *rules* nil)
  (loop for (a b) in orders
        do
           (pushnew (list b) *rules* :key #'car)
           (push a (cdr (assoc b *rules*)))))

(defun sort-update (rules update)
  (stable-sort (copy-list update)
               (lambda (a b)
                 (cond
                   ((member (list a b) rules :test #'equal) t)
                   (t nil)))))

(defun part1 (data)
  (destructuring-bind (orders . updates) data
    (loop for update in updates
          when (equal (sort-update orders update) update)
            sum (nth (floor (length update) 2) update))))  

(defun part2 (data)
  (destructuring-bind (orders . updates) data
    (loop for update in updates
          for correct = (sort-update orders update)
          unless (equal (sort-update orders update) update)
            sum (nth (floor (length update) 2) correct))))
