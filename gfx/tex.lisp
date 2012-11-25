;;;; gfx/tex.lisp
(in-package #:hnh-gfx)

(defstruct tex
  ;;backing-data, should be a png
  (image nil :type imago:image)
  (size nil :type hnh-utils:vec)
  ;;OpenGL
  (glid -1 :type number)
  (real-size nil :type hnh-utils:vec))

(defun make-texture (image)
  "easier make function"
  (make-tex :image image
            :size (make-instance 'vec
                                 :x (imago:image-width image)
                                 :y (imago:image-height image))))

(defun next-pow-2 (n)
  (let ((pow (log n 2)))
    (if (zerop (- (expt 2 pow) pow))
        n ;; already pow-of-2
        (expt 2 (ceiling (log n 2))))))

(defun scale-and-convert-image (tex)
  "Returns an image's pixel data scaled up to the nearest power of
2 width and height"
  (let* ((w (next-pow-2 (imago:image-width (tex-image tex))))
         (h (next-pow-2 (imago:image-height (tex-image tex))))
         (buff (make-array (* w h 4) :element-type '(unsigned-byte 8))))
    (setf tex-real-size (make-instance 'vec :x w :y h))
    (loop for x from 0 to (imago:image-width (tex-image tex))
       do (loop for y from 0 to (imago:image-height (tex-image tex))
             do (setf (aref buff (+ (* x w 4) (* y 4) 0)) 
                      (imago:color-red 
                       (imago:image-pixel (tex-image tex) x y)))
               (setf (aref buff (+ (* x w 4) (* y 4) 0)) 
                      (imago:color-green
                       (imago:image-pixel (tex-image tex) x y)))
               (setf (aref buff (+ (* x w 4) (* y 4) 0)) 
                      (imago:color-blue 
                       (imago:image-pixel (tex-image tex) x y)))
               (setf (aref buff (+ (* x w 4) (* y 4) 0)) 
                      (imago:color-alpha
                       (imago:image-pixel (tex-image tex) x y)))))
    buff))
    
    

(defun create-gltexture (tex)
  "Generates the OpenGL texture from our image"
  (when (null (tex-image tex))
    (error "Attempting to generate a nil texture"))
  (let ((data (scale-and-convert-image tex)))
    (setf (tex-glid tex) (gl:gen-textures 1))
    (gl:bind-texture :texture-2d (tex-glid tex))
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-image-2d :target
                     0
                     :rgba
                     (hnh-utils:x (tex-real-size tex)) 
                     (hnh-utils:y (tex-real-size tex))
                     0
                     :rgba 
                     :gl-unsigned-byte
                     data)))
