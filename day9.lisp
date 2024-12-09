(defun parse-data (path)
  (let* ((s (uiop:read-file-string path))
         (layout (loop for c across s
                       when (not (equal c #\Newline))
                         collect (parse-integer (string c))))
         (length (reduce #'+ layout))
         (fs (make-array length :initial-element nil)))
    (loop for f from 0
          for (file-length free-length . nil) on layout by #'cddr
          and i = 0 then (+ i file-length free-length)
          do
             (loop for ii from i below (+ i file-length)
                   do
                      (setf (aref fs ii) f)))
    fs))

(defun show-data (data)
  (loop for f across data
        do
           (format t "~:[.~;~:*~d~] " f)
        finally
           (format t "~%")))

(defparameter *sample-data* (parse-data "sample/day9.txt"))
(defparameter *data* (parse-data "input/day9.txt"))
