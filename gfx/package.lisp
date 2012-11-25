;;;; gfx/package.lisp

(defpackage #:hnh-gfx
  (:use #:cl)
  (:export
   ;;tex.lisp
   #:make-tex
   #:copy-tex
   #:tex-image
   #:tex-size

   ;;renderer.lisp
   #:make-renderer
   #:copy-renderer
   #:renderer-ul
   #:renderer-size
   #:renderer-color

   #:image
   #:upd-render))
