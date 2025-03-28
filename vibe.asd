(defsystem "vibe"
  :description "A video game called Vibe"
  :version "0.1.0"
  :author "Joseph Marshall"
  :license "MIT"
  :depends-on ("cl-opengl" "cl-utilities" "sdl2" "sdl2-ttf" "sdl2-image")
  :components ((:file "src/main"     :depends-on ("package"))
               (:file "package")))
