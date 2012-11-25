;;;; gfx/renderer.lisp
(in-package #:hnh-gfx)

(defstruct renderer
  ;;location
  (ul (hnh-utils:zero-vector)
      :type hnh-utils:vec)
  ;;size
  (size (hnh-utils:zero-vector)
        :type hnh-utils:vec)
  ;;color
  (color imago:+white+
         :type imago:rgb-pixel))

;; Current texture id, global to all renderers 
(defparameter *cur-tex-id* 0)

(defun vert (coord)
  "Defines a vertex in gl"
  (gl:vertex (x coord) (y coord)))

(defun setenv ()
  (gl:tex-env :texture-env :texture-env-mode :modulate))

(defun blend (rend amb)
  (let ((r (imago:color-red   (renderer-color color)))
        (g (imago:color-green (renderer-color color)))
        (b (imago:color-blue  (renderer-color color)))
        (a (imago:color-alpha (renderer-color color))))
    (imago:make-color (/ (* r (imago:color-red   amb)) 255)
                      (/ (* g (imago:color-green amb)) 255)
                      (/ (* b (imago:color-blue  amb)) 255)
                      (/ (* a (imago:color-alpha amb)) 255))))

(defun select-tex (glid)
  "changes textures to the specified id, -1 for none"
  (if (< glid 0)
      (gl:disable :texture-2d)
      (if (/= *cur-tex-id* glid)
          (gl:enable :texture-2d)))
  (setf *cur-tex-id* glid))

(defun image-2 (rend tex pos ul br sz)
  "Draws an image at specified coords and given size"
  (select-tex (tex-glid tex))
  (with-slots (ul-x ul-y) ul
    (with-slots (real-x real-y) (tex-real-size tex)
      (with-slots (br-x br-y) br
        (with-slots (c-x c-y) pos
          (with-slots (sz-x sz-y) sz
            (let ((l  (/ ul-x real-x))
                  (tt (/ ul-y real-y))
                  (r  (/ br-x real-x))
                  (b  (/ br-y real-y)))
              (gl:color (/ (imago:color-red   (renderer-color rend)) 255)
                        (/ (imago:color-green (renderer-color rend)) 255)
                        (/ (imago:color-blue  (renderer-color rend)) 255)
                        (/ (imago:color-alpha (renderer-color rend)) 255))
              (gl:with-primitive :quads
                (gl:tex-coord l tt) (gl:vertex c-x c-y)
                (gl:tex-coord r tt) (gl:vertex (+ c-x sz-x) c-y)
                (gl:tex-coord r b)  (gl:vertex (+ c-x sz-x) (+ c-y sz-y))
                (gl:tex-coord l b)  (gl:vertex c-x (+ c-y sz-y))))))))))

(defun image-1 (rend tex coord size)
  "Draws an image at specified coord with given size"
  (when (hnh-utils:box-intersect coord (tex-size tex)
                                 (renderer-ul rend)
                                 (renderer-size rend))
    (let ((pos (hnh-utils:copy-vec coord))
          (ul (make-instance 'hnh-utils:vec :x 0 :y 0))
          (br (hnh-utils:copy-vec (tex-size tex)))
          (sz (hnh-utils:copy-vec (tex-real-size tex))))
      (with-slots (c-x c-y) coord
        (with-slots (size-x size-y) size
          (with-slots (pos-x pos-y) pos
            (with-slots (ul-x ul-y) ul
              (with-slots (br-x br-y) br
                (with-slots (sz-x sz-y) sz
                  (with-slots (tsz-x tsz-y) (tex-size tex)
                    ;;fix tex if too far left
                    (when (< c-x ul-x)
                      (let ((pd (- ul-x c-x)))
                        (setf pos-x ul-x
                              ul-x (/ (* pd tsz-x) tsz-x))
                        (decf sz-x pd)))
                    ;;fix tex if too far up
                    (when (< c-y ul-y)
                      (let ((pd (- ul-y c-y)))
                        (setf pos-y ul-y
                              ul-y (/ (* pd tsz-y) tsz-y))
                        (decf sz-y pd)))
                    ;;fix tex if too far right
                    (when (> (+ c-x tsz-x)
                             (+ ul-x size-x))
                      (let ((pd (- (+ c-x tsz-x)
                                   (+ ul-x size-x))))
                        (decf sz-x pd)
                        (decf br-x (/ (* pd tsz-x) tsz-x))))
                    ;;fix tex if too far down
                    (when (> (+ c-y tsz-y)
                             (+ ul-y size-y))
                      (let ((pd (- (+ c-y tsz-y)
                                   (+ ul-y size-y))))
                        (decf sz-y pd)
                        (decf  br-y (/ (* pd tsz-y) tsz-y)))))))))))
      (image-2 rend tex ul br sz))))
                    
    

(defmacro image (rend tex coord &optional (size nil))
  (if (null size)
      `(image-1 ,rend ,tex ,coord ,(tex-size tex))
      `(image-1 ,rend ,tex ,coord ,size)))

(defun upd-render (rend dul size)
  (make-renderer
   :ul (hnh-utils:vec-add (renderer-ul rend)
                          dul)
   :size size
   :color (renderer-color rend)))
