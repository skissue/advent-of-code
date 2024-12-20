(defun parse-data (path)
  (coerce (uiop:read-file-lines path) 'vector))

(defparameter *sample-data* (parse-data "sample/day4.txt"))
(defparameter *data* (parse-data "input/day4.txt"))

(defparameter *dirs* '((0 . 1)
                       (1 . 0)
                       (1 . 1)
                       (0 . -1)
                       (-1 . 0)
                       (-1 . -1)
                       (1 . -1)
                       (-1 . 1)))

(defun recurse-direction (data pos remaining dir)
  (unless remaining
    (return-from recurse-direction t))
  (destructuring-bind ((x . y) (dx . dy)) (list pos dir)
    (let ((nx (+ x dx))
          (ny (+ y dy)))
      (unless (and (< -1 nx (length (aref data 0)))
                   (< -1 ny (length data)))
        (return-from recurse-direction))
      (when (equal (car remaining) (aref (aref data ny) nx))
        (recurse-direction data (cons nx ny) (cdr remaining) dir)))))

(defun part1 (data)
  (loop for y below (length data)
        sum (loop for x below (length (aref data 0))
                  when (equal #\X (aref (aref data y) x))
                    sum (loop for dir in *dirs*
                              when (recurse-direction
                                    data (cons x y) '(#\M #\A #\S) dir)
                                sum 1))))

(defconstant +diags+ '((1 . 1)
                       (-1 . -1)
                       (1 . -1)
                       (-1 . 1)))

(defun get-diag (data pos dir)
  (destructuring-bind ((x . y) . (dx . dy)) (cons pos dir)
    (let ((nx (+ x dx))
          (ny (+ y dy)))
      (unless (and (< -1 nx (length (aref data 0)))
                   (< -1 ny (length data)))
        (return-from get-diag))
      (aref (aref data ny) nx))))

(defun part2 (data)
  (loop for y below (length data)
        sum (loop for x below (length (aref data 0))
                  when (and (equal #\A (aref (aref data y) x))
                            (member (loop for diag in +diags+
                                          collect (get-diag data (cons x y) diag))
                                    '((#\M #\S #\M #\S)
                                      (#\M #\S #\S #\M)
                                      (#\S #\M #\M #\S)
                                      (#\S #\M #\S #\M))
                                    :test #'equal))
                    sum 1)))

