(in-package :cl-user)
(defpackage game-of-life.model
  (:use :cl)
  (:import-from :local-time
                :timestamp-year
                :timestamp-month
                :timestamp-day
                :timestamp-hour
                :timestamp-minute
                :timestamp-second)
  (:export :init-board
           :refresh-board
           :update-board
           :read-board
           :save-board
           :existp
           :birth
           :kill
           :invert-cell
           :random-board))
(in-package :game-of-life.model)

(defvar *x-range*)
(defvar *y-range*)
(defvar *board*)

(defun init-board (x-range y-range)
  "Initialize board environment. Ranges are the number of cells in one side."
  (setf *x-range* x-range)
  (setf *y-range* y-range)
  (setf *board*
        (make-array (* x-range y-range)
                    :element-type 'boolean
                    :initial-element nil))
  t)

(defun refresh-board ()
  "Killing all cells"
  (dotimes (i (* *x-range* *y-range*))
    (setf (aref *board* i) nil)))

(defun in-board-p (x y)
  "Points are in board or not."
  (and (<= 0 x) (< x *x-range*)
       (<= 0 y) (< y *y-range*)))

(defun existp (x y)
  "If a cell at (x y) is alive, this function will return t."
  (when (in-board-p x y)
    (aref *board* (to-index x y))))

(defun to-index (x y)
  "Convert a point in x-y plane to array's index."
  (+ (* y *x-range*) x))

(defun birth (x y)
  "A cell at (x y) will be alive."
  (when (in-board-p x y)
    (setf (aref *board* (to-index x y)) t)))

(defun kill (x y)
  "A cell at (x y) will be dead."
  (when (in-board-p x y)
    (setf (aref *board* (to-index x y)) nil)))

(defun invert-cell (x y)
  "Invert alive/dead of a cell at (x y)."
  (if (existp x y)
      (kill x y)
      (birth x y)))

(defun neighbor-locations (x y)
  "Return 8 points of around (x y)."
  (remove-if-not
   (lambda (location)
     (in-board-p (car location) (cdr location)))
   (list (cons (1- x) (1- y))
         (cons (1- x) (1+ y))
         (cons (1- x) y)
         (cons (1+ x) (1- y))
         (cons (1+ x) (1+ y))
         (cons (1+ x) y)
         (cons x (1- y))
         (cons x (1+ y)))))

(defun count-neighbor (x y)
  "Count the alive cells around (x y)."
  (count-if
   (lambda (location)
     (aref *board* (to-index (car location) (cdr location))))
   (neighbor-locations x y)))

(defun next-status (x y)
  "Return next status (alive/dead) of a cell at (x y)."
  (let ((c (count-neighbor x y)))
    (if (existp x y)
        (or (= c 2) (= c 3))
        (= c 3))))

(defun update-board ()
  "Update board to next step."
  (let ((next (copy-seq *board*)))
    (dotimes (x *x-range*)
      (dotimes (y *y-range*)
        (setf (aref next (to-index x y))
              (next-status x y))))
    (setf *board* next))
  t)

(defun random-board ()
  "Invert the quarter of all cells' state."
  (let ((count (floor (/ (* *x-range* *y-range*) 4))))
    (loop :repeat count :do
       (invert-cell (random *x-range*)
                    (random *y-range*)))))

(defun read-board (path)
  "Read initial state of board from file."
  (with-open-file (stream path :direction :input)
    (let* ((cell-list (read stream t nil)))
      (loop :for (x . y) :in cell-list :do
         (birth x y)))))

(defun save-board ()
  "Write current state of board to file. The file will write in save/YYYYMMDDhhmmss.lisp."
  (let* ((now (local-time:now))
         (path (format nil "~A/save/~A~A~A~A~A~A.lisp"
                       (asdf:system-source-directory :game-of-life)
                       (timestamp-year now)
                       (timestamp-month now)
                       (timestamp-day now)
                       (timestamp-hour now)
                       (timestamp-minute now)
                       (timestamp-second now))))
    (with-open-file (stream path :direction :output)
      (format stream "~S" (let (out)
                            (dotimes (i *x-range*)
                              (dotimes (j *y-range*)
                                (when (existp i j)
                                  (push (cons i j) out))))
                            out)))))
