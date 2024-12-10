(ql:quickload :cl-ppcre)

(defun process-data (path)
  (mapcar (lambda (x)
            (mapcar #'parse-integer (cl-ppcre:split " " x)))
          (uiop:read-file-lines path)))

(defparameter *sample-data* (process-data "sample/day2.txt"))
(defparameter *data* (process-data "input/day2.txt"))

(defun valid-report (report)
  (let ((comp (comp report)))
    (loop for a in report
          for b in (cdr report)
          always (funcall comp a b)
          always (<= 1 (abs (- a b)) 3))))

(defun part1 (data)
  (length (remove-if-not #'valid-report data)))

(defun comp (report)
  (if (> (car report)
         (cadr report))
      #'> #'<))

(defun remove-nth (n list)
  (append (subseq list 0 n) (nthcdr (1+ n) list)))

(defun valid-report-2 (report)
  (or (valid-report report)
      (loop for i from 0 to (length report)
            thereis (valid-report (remove-nth i report)))))

(defun part2 (data)
  (length (remove-if-not #'valid-report-2 data)))
