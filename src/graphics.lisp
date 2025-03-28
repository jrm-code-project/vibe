(in-package :vibe.graphics)


(defun initialize-graphics (renderer)
  )

(defun load-number-textures (renderer)
  )

(defun load-other-textures (renderer)
  )

(defun unload-number-textures ()
  )

(defun unload-other-textures ()
  )

(defun render-frame (renderer resources)
  ;; Render a single frame
  (sdl2:render-clear renderer)
  (vibe.game:render-game renderer resources)
  (sdl2:render-present renderer))

(defun quit-graphics ()
  )

