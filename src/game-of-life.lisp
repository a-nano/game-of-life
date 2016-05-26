(in-package :cl-user)
(defpackage game-of-life
  (:use :cl)
  (:import-from :game-of-life.model
                :init-board
                :refresh-board
                :update-board
                :birth
                :kill
                :existp
                :read-board
                :save-board
                :random-board)
  (:export :start-game))
(in-package :game-of-life)

(defvar *window-size*)
(defvar *one-side-cells*)
(defvar *auto-mode*)

(defun draw-cells ()
  "Draw alive cells"
  (let ((cell-size (/ *window-size* *one-side-cells*)))
    (dotimes (x *one-side-cells*)
      (dotimes (y *one-side-cells*)
        (when (existp x y)
          (sdl:draw-box-* (floor (* x cell-size))
                          (floor (* y cell-size))
                          (1- (floor cell-size))
                          (1- (floor cell-size))
                          :color sdl:*green*))))))

(defun draw-grid ()
  "Draw grid to board."
  (let ((cell-size (/ *window-size* *one-side-cells*))
        (color (sdl:color :r 50 :g 50 :b 50 :a 1)))
    (dotimes (i *one-side-cells*)
      (sdl:draw-line-* 0
                       (1- (floor (* cell-size i)))
                       *window-size*
                       (1- (floor (* cell-size i)))
                       :color color)
      (sdl:draw-line-* (1- (floor (* cell-size i)))
                       0
                       (1- (floor (* cell-size i)))
                       *window-size*
                       :color color))))

(defun to-cell-location (mouse-point)
  "Convert window scale point to cell scale point."
  (let ((cell-size (/ *window-size* *one-side-cells*)))
    (floor (/ mouse-point cell-size))))

(defun start-game (&key
                     (window-size 640)
                     (one-side-cells 80)
                     (frame-rate 10)
                     (read-path nil))
  "Start game-of-life."
  ;;setup environment
  (setf *auto-mode* nil)
  (setf *window-size* window-size)
  (setf *one-side-cells* one-side-cells)
  (init-board one-side-cells one-side-cells)
  (when read-path
    (read-board read-path))

  (sdl:with-init ()
    (sdl:window *window-size* *window-size*
                :title-caption "LIFE GAME")
    (setf (sdl:frame-rate) frame-rate)
    (sdl:initialise-default-font sdl:*font-10x20*)
    (sdl:update-display)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
        (case key
          ;; quit game
          (:sdl-key-q
           (sdl:push-quit-event))
          ;; to move on to the next step
          (:sdl-key-return
           (update-board)
           (draw-cells)
           (sdl:update-display))
          ;; to enable/disable auto step mode
          (:sdl-key-space
           (if *auto-mode*
               (setf *auto-mode* nil)
               (setf *auto-mode* t)))
          ;; kill all cells
          (:sdl-key-d
           (refresh-board)
           (setf *auto-mode* nil))
          ;; make random alive cells
          (:sdl-key-r
           (random-board))
          ;; save board to save/YYYYMMDDhhmmss.lisp
          (:sdl-key-s
           (save-board))))
      (:mouse-button-down-event (:button button :x x :y y)
        (case button
          (1
           (birth (to-cell-location x)
                  (to-cell-location y)))
          (3
           (kill (to-cell-location x)
                 (to-cell-location y))))
        (draw-cells)
        (sdl:update-display))
      (:mouse-motion-event (:state state :x x :y y)
        (unless (zerop state)
          (case state
            (1
             (birth (to-cell-location x)
                    (to-cell-location y)))
            (4
             (kill (to-cell-location x)
                   (to-cell-location y))))
          (draw-cells)
          (sdl:update-display)))
      (:idle ()
        (sdl:clear-display sdl:*black*)        
        
        (draw-cells)
        (if *auto-mode*
            (update-board)
            (draw-grid))
        
        (sdl:update-display)))))
