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

;; alpha*ax + beta*bx = px
;; alpha*ay + beta*by = py
(defun solve-machine (data)
  (destructuring-bind (ax ay bx by px py) data
    (let* ((scale (/ ax ay))           
           (by1 (* by scale))
           (py1 (* py scale))
           (beta (/ (- px py1) (- bx by1)))
           (alpha (/ (- px (* beta bx)) ax)))
      (values alpha beta))))

(defun part1 (data)
  (loop for machine in data
        for (alpha beta) = (multiple-value-list (solve-machine machine))
        when (and (integerp alpha) (integerp beta))
          sum (+ (* (round alpha) 3)
                 (round beta))))

(defun part2 (data)
  (loop for (ax ay bx by px py) in data
        for (alpha beta) = (multiple-value-list
                            (solve-machine (list ax ay bx by (+ px 10000000000000) (+ py 10000000000000))))
        when (and (integerp alpha) (integerp beta))
          sum (+ (* (round alpha) 3)
                 (round beta))))
