(defun parse-data (path)
  (let (data)
    (cl-ppcre:do-register-groups (x y vx vy)
        ("p=(.*),(.*) v=(.*),(.*)\\n" (uiop:read-file-string path) data)
      (push (cons (cons (parse-integer x)
                        (parse-integer y))
                  (cons (parse-integer vx)
                        (parse-integer vy)))
            data))))

(defparameter *sample-data* (parse-data "sample/day14.txt"))
(defparameter *data* (parse-data "input/day14.txt"))

(defparameter *time* 100)
(defparameter *grid* (cons 101 103))

(defun part1 (data)
  (let* ((quad1 0)
         (quad2 0)
         (quad3 0)
         (quad4 0)
         (width (car *grid*))
         (height (cdr *grid*))
         (half-width (truncate width 2))
         (right (- width half-width))
         (half-height (truncate height 2))
         (upper (- height half-height)))
    (loop for ((x0 . y0) . (dx . dy)) in data
          for xf = (mod (+ x0 (* dx *time*)) width)
          for yf = (mod (+ y0 (* dy *time*)) height)
          do
             (cond
               ((and (< xf half-width) (< yf half-height))
                (incf quad2))
               ((and (>= xf right) (< yf half-height))
                (incf quad1))
               ((and (< xf half-width) (>= yf upper))
                (incf quad3))
               ((and (>= xf right) (>= yf upper))
                (incf quad4))))
    (* quad1 quad2 quad3 quad4)))

(defun print-grid (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (loop for y below height
          do
             (loop for x below width
                   do
                      (format t "~a" (if (aref grid x y) "O" ".")))
             (format t "~%"))))

;; Had literally zero idea what I was looking for (wish there was a bit better
;; of a description, but perhaps I'm just stupid), so went online for some
;; hints, and apparently the picture is formed when no robots overlap, so that's
;; the check I'm using ¯\_(ツ)_/¯ (apparently some people are doing statistical
;; tests too though, that's probably what I would've eventually done had I been
;; completely clueless).
(defun part2 (data)
  (let* ((width (car *grid*))
         (height (cdr *grid*))
         (grid (make-array (list width height) :initial-element nil)))
    (loop for ((x0 . y0) . (dx . dy)) in data
          do
             (push (cons dx dy) (aref grid x0 y0)))
    (loop
      for i from 1 to 10000
      for new-grid = (make-array (list width height) :initial-element nil)
      do
         (loop for x below width
               do
                  (loop for y below height
                        do
                           (loop for (dx . dy) in (aref grid x y)
                                 for nx = (mod (+ x dx) width)
                                 for ny = (mod (+ y dy) height)
                                 do
                                    (push (cons dx dy) (aref new-grid nx ny)))))
         (setf grid new-grid)
      when (loop for x below width
                 always
                 (loop for y below height
                       always (< (length (aref grid x y)) 2)))
        do (print-grid grid)
           (format t "iteration ~a~%" i))))


