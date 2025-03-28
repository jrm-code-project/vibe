(in-package "VIBE")

(defun render-frame (renderer mine-texture flag-texture)
  (sdl2:set-render-draw-color renderer 0 0 0 0)
  (sdl2:render-clear renderer)
  ;; Render game state
  (sdl2:set-render-draw-color renderer 255 255 255 0)
  (loop for row from 0 below 15
        do (loop for col from 0 below 25
                 do (render-tile renderer row col mine-texture flag-texture)))
  (sdl2:render-present renderer))

(defun render-tile (renderer row col mine-texture flag-texture)
  (declare (ignore mine-texture flag-texture))
  (sdl2:render-draw-rect renderer
                         (sdl2:make-rect (* col 32) (* row 32) 32 32)))

(defun game-loop (window renderer mine-texture flag-texture)
  (declare (ignore window))
  ;; Main game loop
  (sdl2:with-event-loop (:method :poll)
    (:idle ()
           ;; Game logic and rendering
           (render-frame renderer mine-texture flag-texture)
           (sdl2:delay 16))

    (:keydown (:keysym keysym)
      (cond ((eql (sdl2:scancode keysym) :scancode-x)
             (sdl2:push-quit-event))
            ;; suppress output for these known keys
            ((member (sdl2:scancode keysym) '(:scancode-backspace
                                              :scancode-escape
                                              :scancode-left
                                              :scancode-return
                                              :scancode-right
                                              :scancode-space
                                              :scancode-up))
             nil)
            (t
             (format t "~&Keydown: ~s~%" (sdl2:scancode keysym))
             (force-output))))
    (:quit ()
           t)))

(defun initialize ()
  (let ((mine-surface nil)
        (flag-surface nil))
    (unwind-protect
         (progn
           (setq mine-surface (sdl2-image:load-image
                               (asdf:system-relative-pathname "vibe" "textures/mine.png")))
      
           (setq flag-surface (sdl2-image:load-image
                               (asdf:system-relative-pathname "vibe" "textures/flag.png")))

           (sdl2:with-window (window
                              :title "Vibe"
                              :x 0 :y 0
                              :w (* 32 25)
                              :h (* 32 15)
                              :flags '(:shown))
             (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
               (let ((mine-texture nil)
                     (flag-texture nil))
                 (unwind-protect
                      (progn
                        (setq mine-texture (sdl2:create-texture-from-surface renderer mine-surface))
                        (setq flag-texture (sdl2:create-texture-from-surface renderer flag-surface))
                        (game-loop window renderer mine-texture flag-texture))
                   (when flag-texture (sdl2:destroy-texture flag-texture))
                   (when mine-texture (sdl2:destroy-texture mine-texture)))))))
      (when flag-surface (sdl2:free-surface flag-surface))
      (when mine-surface (sdl2:free-surface mine-surface)))))

(defun main ()
  (sdl2:with-init (:video)
    (unwind-protect
         (progn
           (sdl2-image:init '(:png))
           (unwind-protect
                (progn
                  (sdl2-ttf:init)
                  (initialize))
             (sdl2-ttf:quit)))
      (sdl2-image:quit))))
