(use-package :3d-matrices)

(defun parse-data (path)
  (let (data)
    (cl-ppcre:do-register-groups (ax ay bx by px py)
        ("Button A: X\\+(.*), Y\\+(.*)\\nButton B: X\\+(.*), Y\\+(.*)\\nPrize: X=(.*), Y=(.*)"
         (uiop:read-file-string path)
         data)
      (push (mapcar #'parse-integer (list ax ay bx by px py))
            data))))

(defparameter *sample-data* (parse-data "sample/day13.txt"))
(defparameter *data* (parse-data "input/day13.txt"))

(defun solve-machine (data)
  (destructuring-bind (ax ay bx by px py) data
    (let ((a (mat2 (list ax bx ay by)))
          (b (matn 2 1 (list px py))))
      (m* (minv a) b))))

(defun fintegerp (num)
  (< (abs
      (- num
         (fround num)))
     0.0001))

(defun part1 (data)
  (loop for machine in data
        for solution = (solve-machine machine)
        for a = (miref solution 0)
        for b = (miref solution 1)
        when (and (fintegerp a) (fintegerp b))
          sum (+ (* (round a) 3)
                 (round b))))
