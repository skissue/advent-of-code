(ql:quickload :cl-ppcre)

(defparameter *data* (mapcar
                      (lambda (x) (mapcar #'parse-integer (cl-ppcre:split " {3}" x)))
                      (uiop:read-file-lines "input/day1.txt")))

(defun part1 (data)
  (let ((left (sort (mapcar #'car data) #'<))
        (right (sort (mapcar #'cadr data) #'<)))
    (reduce #'+ (mapcar #'abs (mapcar #'- left right)))))

