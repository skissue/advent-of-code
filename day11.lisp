(import 'org.tfeb.hax.memoize:def-memoized-function)

(defparameter *sample-data* '(125 17))
(defparameter *data*
  (mapcar #'parse-integer
          (cl-ppcre:split " " (uiop:read-file-string "input/day11.txt"))))

(defun count-digits (n)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum n))
  (if (= n 0)
      1
      (1+ (floor (log n 10)))))

(defun part-digits (n digits)
  (let ((group (/ digits 2)))
    (multiple-value-list (floor n (expt 10 group)))))

(defun step-stone (stone)
  (let ((digits (count-digits stone)))
    (cond
      ((= stone 0) (list 1))
      ((evenp digits) (part-digits stone digits))
      (t (list (* stone 2024))))))

(defun blink (data)
  (let (new)
    (dolist (stone data)
      (dolist (new-stone (step-stone stone))
        (push new-stone new)))
    (nreverse new)))

(defun part1 (data)
  (loop repeat 26
        for stones = data then (blink stones)
        finally
           (return (length stones))))

(def-memoized-function count-expansion (stone iterations)
  (if (= ))
  (loop repeat (1+ iterations)
        for stones = (list stone) then (blink stones)
        finally
           (return (length stones))))

(defun part2 (data)
  (loop for i from 0 to 38
        for stones = data then (blink stones)
        do (format t "iteration ~d, length ~d~%"
                   i (length stones))
        finally
           (return (length stones))))
