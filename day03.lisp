(ql:quickload :cl-ppcre)

(defparameter *sample-data* (uiop:read-file-string "sample/day3.txt"))
(defparameter *data* (uiop:read-file-string "input/day3.txt"))

(defun part1 (data)
  (let ((sum 0))
    (cl-ppcre:do-register-groups (a b) ("mul\\((\\d{1,3}),(\\d{1,3})\\)" data)
      (setf sum (+ sum (* (parse-integer a) (parse-integer b)))))
    sum))

(defun part2 (data)
  (let ((sum 0)
        (enabled t))
    (cl-ppcre:do-register-groups (a b x) ("mul\\((\\d{1,3}),(\\d{1,3})\\)|(do(?:n't)?)\\(\\)" data)
      (cond
        ((equal x "do") (setf enabled t))
        ((equal x "don't") (setf enabled nil))
        (enabled (setf sum (+ sum (* (parse-integer a) (parse-integer b)))))))
    sum))
