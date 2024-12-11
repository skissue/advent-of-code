(defun parse-data (path)
  (let ((lines (uiop:read-file-lines path))
        (antennas nil))
    (loop for y below (length lines)
          do (loop for x below (length (car lines))
                   for char = (aref (nth y lines) x)
                   when (not (equal char #\.))
                     do
                        (let ((old (cdr (assoc char antennas))))
                          (if old
                              (setf (cdr (assoc char antennas)) (cons (cons x y) old))
                              (setf antennas (acons char (list (cons x y)) antennas))))))
    (cons antennas (cons (length (car lines)) (length lines)))))

(defparameter *sample-data* (parse-data "sample/day8.txt"))
(defparameter *data* (parse-data "input/day8.txt"))

(defun proj (a b bounds)
  (let ((nx (+ (car a) (car a) (- (car b))))
        (ny (+ (cdr a) (cdr a) (- (cdr b)))))
    (when (and (< -1 nx (car bounds))
               (< -1 ny (cdr bounds)))
      (cons nx ny))))

(defun part1 (data)
  (destructuring-bind (antennas . bounds) data
    (let ((antinodes))
      (loop for (nil . locations) in antennas
            do (loop for (target . rest) on locations
                     do (loop for l in rest
                              do
                                 (push (proj target l bounds) antinodes)
                                 (push (proj l target bounds) antinodes))))
      (length (remove-duplicates (remove nil antinodes) :test #'equal)))))

(defun extended-proj (a b bounds)
  (loop with dx = (- (car a) (car b))
        with dy = (- (cdr a) (cdr b))
        for nx = (car a) then (+ nx dx)
        for ny = (cdr a) then (+ ny dy)
        while (and (< -1 nx (car bounds))
                   (< -1 ny (cdr bounds)))
        collect (cons nx ny)))

(defun part2 (data)
  (destructuring-bind (antennas . bounds) data
    (let ((antinodes))
      (loop for (nil . locations) in antennas
            do (loop for (target . rest) on locations
                     do (loop for l in rest
                              do
                                 (setf antinodes (append (extended-proj target l bounds)
                                                         antinodes))
                                 (setf antinodes (append (extended-proj l target bounds)
                                                         antinodes)))))
      (length (remove-duplicates antinodes :test #'equal)))))
