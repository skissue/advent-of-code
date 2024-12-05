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

(defun part1 (data)
  (destructuring-bind (orders . updates) data
    (fill-rules orders)
    (loop for update in updates
          when (loop for x in update
                     for i from 0
                     always (loop for a in (subseq update 0 i)
                                  never (member x (assoc  a *rules*))))
            sum (nth (ceiling (length update) 2) update))))
