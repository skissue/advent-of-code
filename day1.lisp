(ql:quickload :cl-ppcre)

(defparameter *data* (mapcar
                      (lambda (x) (mapcar #'parse-integer (cl-ppcre:split " {3}" x)))
                      (uiop:read-file-lines "input/day1.txt")))

(defun part1 (data)
  (let ((left (sort (mapcar #'car data) #'<))
        (right (sort (mapcar #'cadr data) #'<)))
    (reduce #'+ (mapcar #'abs (mapcar #'- left right)))))

(defparameter *counts* (make-hash-table))

(defun part2-generate-counts (data)
  (setf *counts* (make-hash-table))
  (let ((right (mapcar #'cadr data)))
    (loop for x in right
          for cur = (gethash x *counts* 0)
          do
             (setf (gethash x *counts*) (1+ cur)))))

(defun part2 (data)
  (part2-generate-counts data)
  (reduce #'+
          (mapcar (lambda (x) (* x (gethash x *counts* 0)))
                  (mapcar #'car data))))
