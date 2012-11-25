;;;; hnh-client.lisp

(in-package #:hnh-client)

(defun init-gl ()
  (gl:color 1.0 1.0 1.0)
  (gl:point-size 4)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (read-in-specs))

;;;glfw callbacks and mouse constants
(defun key-fun (key state)
  ;(format t "KEY: ~A[~A]| STATE: ~A~%" key (type-of key) state)
  ;;;XXX : Maybe make everything strings instead...
  (when (stringp key)
    (setf key (intern key)))
  (if (= state glfw:+press+)
      (hnh-ui:on-key-down key)
      (hnh-ui:on-key-up   key)))

(defun char-fun (char state)
  ;(format t "CHAR: ~A | STATE ~A~%" char state)
  (hnh-ui:on-char char))

(let ((last-x 0)
      (last-y 0))
  (defun mouse-pos-fun (x y)
    ;(format t "MOUSE-MOVE : ~A, ~A~%" x y)
    (hnh-ui:on-mouse-move x y (- x last-x) (- y last-y))
    (setf last-x x)
    (setf last-y y)))
  
(defun mouse-btn-fun (btn state)
  ;(format t "BTN: ~A | STATE: ~A~%" btn state)
  (destructuring-bind (x y) (glfw:get-mouse-pos) 
    (if (= state glfw:+press+)
        (hnh-ui:on-mouse-down btn x y)
        (hnh-ui:on-mouse-up   btn x y))))
  
(let ((last-pos 0))
  (defun mouse-wheel-fun (pos)
    ;(format t "POSITION: ~A~%" pos)
    (destructuring-bind (x y) (glfw:get-mouse-pos)
      (hnh-ui:on-mouse-wheel pos (- pos last-pos) x y))
    (setf last-pos pos)))

(defun resize-fun (w h)
  ;(format t "RESIZE: ~A, ~A~%" w h)
  )


(defun draw ()
  ;;render lighting
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 *window-width* 0 *window-height* -1 1)
  
  ;;setup for ui
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 *window-width* *window-height* 0 -1 1)
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer-bit)

  
  ;;render ui
  (hnh-ui:draw-ui)
  ;;render tooltip
  ;;render cursor
  )
  

(defun main (args)
  (parse-args args)
  (hnh-ui:reset-ui)
  (glfw:do-window (:title +title+ :width *window-width* :height *window-height*
                          :redbits 8 :greenbits 8 :bluebits 8 :alphabits 8)
      ((glfw:set-window-size *window-width* *window-height*)
       (glfw:set-window-title +title+)
       (glfw:swap-interval 1)
       ;;setup callbacks
       (glfw:set-window-size-callback 'resize-fun)
       (glfw:set-key-callback 'key-fun)
       (glfw:set-char-callback 'char-fun)
       (glfw:set-mouse-button-callback 'mouse-btn-fun)
       (glfw:set-mouse-pos-callback 'mouse-pos-fun)
       (glfw:set-mouse-wheel-callback 'mouse-wheel-fun)
       ;;init opengl
       (init-gl))
    (draw)))
