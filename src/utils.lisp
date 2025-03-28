(in-package :vibe.utils)

(defmacro with-ttf (&body body)
  "Initialize SDL2-TTF on entry and ensure cleanup with unwind-protect."
  `(unwind-protect
       (progn
         (sdl2-ttf:init)
         ,@body)
     (sdl2-ttf:quit)))

(defmacro with-sdl2-image (image-formats &body body)
  "Initialize SDL2-Image on entry and ensure cleanup with unwind-protect."
  `(unwind-protect
       (progn
         (sdl2-image:init ',image-formats)
         ,@body)
     (sdl2-image:quit)))

(defmacro with-surfaces ((&rest surface-bindings) &body body)
  "Load surfaces, execute the body, and destroy the surfaces afterward.
Each binding in SURFACE-BINDINGS should be of the form (var path), where VAR is the variable to bind the surface to, and PATH is the relative path to the texture file."
  (let ((surface-vars (mapcar #'first surface-bindings)))
    `(let ,(mapcar (lambda (binding)
                     `(,(first binding) (sdl2-image:load-image (asdf:system-relative-pathname "vibe" ,(second binding)))))
                   surface-bindings)
       (unwind-protect
           (progn
             ,@body)
         ,@(mapcar (lambda (var)
                     `(sdl2:free-surface ,var))
                   surface-vars)))))

(defmacro with-textures ((renderer (&rest texture-bindings)) &body body)
  "Create textures from surfaces using the given renderer, execute the body, and destroy the textures afterward.
Each binding in TEXTURE-BINDINGS should be of the form (var surface), where VAR is the variable to bind the texture to, and SURFACE is the surface to create the texture from."
  (let ((texture-vars (mapcar #'first texture-bindings)))
    `(let (,@(mapcar (lambda (binding)
                     `(,(first binding) (sdl2:create-texture-from-surface ,renderer ,(second binding))))
                   texture-bindings))
       (unwind-protect
           (progn
             ,@body)
         ,@(mapcar (lambda (var)
                     `(sdl2:destroy-texture ,var))
                   texture-vars)))))
