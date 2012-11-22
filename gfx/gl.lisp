;;;; gfx/gl.lisp
(in-package #:hnh-gfx

(defun vert (coord)
  "Defines a vertex in gl"
  (gl:vertex (vec2n-x coord) (vec2n-y coord)))

(defun check-error ()
  "Checks for any opengl errors"
  (let ((err (gl:get-error)))
    (when (not (zerop err))
      (error (format nil "~A OpenGL error." err)))))
