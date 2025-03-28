(in-package :vibe.game)

(defparameter *board* nil)
(defparameter *rows* 15) ;; Number of rows on the board
(defparameter *cols* 25) ;; Number of columns on the board
(defparameter *tile-size* 32) ;; Updated tile size to a power of 2
(defparameter *mines* 50)

(defun initialize-game ()
  ;; Initialize game state
  (setf *board* (make-array (list *rows* *cols*) :initial-element 0))
  (place-mines))

(defun place-mines ()
  ;; Randomly place mines on the board
  (loop repeat *mines*
        do (let ((row (random *rows*))
                 (col (random *cols*)))
             (unless (eql (aref *board* row col) :mine)
               (setf (aref *board* row col) :mine)
               (update-adjacent-cells row col)))))

(defun update-adjacent-cells (row col)
  ;; Update the numbers in adjacent cells
  (loop for i from (max 0 (1- row)) to (min (1+ row) (1- *rows*))
        do (loop for j from (max 0 (1- col)) to (min (1+ col) (1- *cols*))
                 do (unless (eql (aref *board* i j) :mine)
                      (incf (aref *board* i j))))))

(defun update-game ()
  ;; Update game state
  )

(defun render-game (renderer resources)
  ;; Render game state
  (loop for row from 0 below *rows*
        do (loop for col from 0 below *cols*
                 do (render-tile renderer row col resources))))

(defun render-tile (renderer row col resources)
  (when (and *board* (arrayp *board*)) ;; Ensure *board* is initialized and is an array
    (let ((tile (aref *board* row col))
          (texture (case (aref *board* row col)
                     (:mine (getf resources :mine))
                     (:flag (getf resources :flag))
                     (0 (getf resources :blank))
                     (1 (getf resources :one))
                     (2 (getf resources :two))
                     (3 (getf resources :three))
                     (4 (getf resources :four))
                     (5 (getf resources :five))
                     (6 (getf resources :six))
                     (7 (getf resources :seven))
                     (8 (getf resources :eight)))))
      (when texture
        (sdl2:render-copy renderer texture
                          :source-rect (sdl2:make-rect 0 0 *tile-size* *tile-size*)
                          :dest-rect (sdl2:make-rect (* col *tile-size*) (* row *tile-size*) *tile-size* *tile-size*))))))
