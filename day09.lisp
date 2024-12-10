(defun parse-data (path)
  (let* ((s (uiop:read-file-string path))
         (layout (loop for c across s
                       when (not (equal c #\Newline))
                         collect (parse-integer (string c))))
         (length (reduce #'+ layout))
         (fs (make-array length :initial-element -1)))
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
           (format t "~a " (if (= -1 f) "." f))
        finally
           (format t "~%")))

(defparameter *sample-data* (parse-data "sample/day9.txt"))
(defparameter *data* (parse-data "input/day9.txt"))

(defun checksum (data)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) data))
  (loop for i of-type fixnum from 0
        for x of-type fixnum across data
        when (<= 0 x)
          sum (the fixnum (* i x)) of-type fixnum))

(defun part1 (data)
  (loop with i = 0
        with j = (1- (length data))
        while (< i j)
        if (<= 0 (aref data i)) do
          (incf i)
        else
          if (= -1 (aref data j)) do
            (decf j)
        else do
          (rotatef (aref data i) (aref data j))
          (incf i)
          (decf j))
  (checksum data))

(defun free-size (data index)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) data)
           (type unsigned-byte index))
  (loop for i from index
        while (= -1 (aref data i))
        count t))

(defun file-size (data index)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) data)
           (type unsigned-byte index))
  (loop with file-id = (aref data index)
        for i downfrom index
        while (equal (aref data i) file-id)
        count t))

(defun part2 (data)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) data))
  (loop with i of-type unsigned-byte = (1- (length data))
        with filled = 0
        while (> i filled)
        when (<= 0 (aref data i)) do
          (let* ((size (file-size data i))
                 (target (loop with zeroes = t
                               for j of-type (unsigned-byte 64) from filled below i
                               for free of-type (unsigned-byte 64) = (free-size data j)
                               when (>= free size)
                                 return j
                               when (not (= free 0))
                                 do (setf zeroes nil)
                               when zeroes
                                 do (setf filled j))))
            (declare (type (unsigned-byte 64) size))
            (when target
              (loop repeat size
                    for ii downfrom i
                    for jj from target
                    do
                       (rotatef (aref data ii) (aref data jj))))
            (decf i size))
        else do (decf i))
  (checksum data))
