(ql:quickload :cl-ppcre)

(defun process-data (path)
  (mapcar (lambda (x)
            (mapcar #'parse-integer (cl-ppcre:split " " x)))
          (uiop:read-file-lines path)))

(defparameter *sample-data* (process-data "sample/day2.txt"))
(defparameter *data* (process-data "input/day2.txt"))

(defun valid-report (report)
  (let ((comp (if (> (car report)
                     (cadr report))
                  #'> #'<)))
    (loop for a in report
          for b in (cdr report)
          always (funcall comp a b)
          always (<= 1 (abs (- a b)) 3))))

(defun part1 (data)
  (length (remove-if-not  #'valid-report data)))
