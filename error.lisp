;;;; error.lisp

(in-package #:hnh-client)


;;;Specs
(defparameter *gl-vendor*   nil)
(defparameter *gl-version*  nil)
(defparameter *gl-renderer* nil)
(defparameter *gl-exts*     nil)
(defparameter *gl-shader*   nil)

(defun read-in-specs ()
  (setf *gl-vendor*   (gl:get-string :vendor))
  (setf *gl-version*  (gl:get-string :version))
  (setf *gl-renderer* (gl:get-string :renderer))
  (setf *gl-exts*     (gl:get-string :extensions))
  (setf *gl-shader*   (gl:get-string :shading-language-version)))

(defun print-error-specs ()
  (format t "GL_VENDOR: ~A~%
GL_VERSION: ~A~%
GL_RENDERER: ~A~%
GL_EXTS: ~A~%
GL_SHADER_VERISON: ~A~%" *gl-vendor* *gl-version* *gl-renderer* *gl-exts*
*gl-shader*))
