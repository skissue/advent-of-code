(ql:quickload :cl-ppcre)
(ql:quickload :lparallel)

(setf lparallel:*kernel* (lparallel:make-kernel 12))

(defun parse-data (path)
  (mapcar (lambda (x)
            (mapcar #'parse-integer
                    (cl-ppcre:split ":? " x)))  
          (uiop:read-file-lines path)))

(defparameter *sample-data* (parse-data "sample/day7.txt"))
(defparameter *data* (parse-data "input/day7.txt"))

(defun calc-valid-p (target cur remaining)
  (if (null remaining)
      (= cur target)
      (destructuring-bind (x . xs) remaining
        (or (calc-valid-p target (+ cur x) xs)
            (calc-valid-p target (* cur x) xs)))))

(defun part1 (data)
  (loop for (target . nums) in data
        when (calc-valid-p target 0 nums)
          sum target))

(defun concat (a b)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum a b))
  (let* ((digits (1+ (floor (log b 10))))
         (padding (expt 10 digits))
         (shifted (* a padding)))
    (declare (type fixnum digits padding shifted))
    (the fixnum (+ shifted b))))

(defun calc-valid-p-2 (target cur remaining)
  (if (null remaining)
      (= cur target)
      (destructuring-bind (x . xs) remaining
        (or (calc-valid-p-2 target (+ cur x) xs)
            (calc-valid-p-2 target (* cur x) xs)
            (calc-valid-p-2 target (concat cur x) xs)))))

(defun part2 (data)
  (lparallel:pmap-reduce
   (lambda (d)
     (destructuring-bind (target . (start . nums)) d
       (if (calc-valid-p-2 target start nums)
           target 0)))
   #'+
   data))
