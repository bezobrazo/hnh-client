;;;; ui/ui.lisp

(in-package #:hnh-ui)

;;;Event-related
(defparameter *mod-ctrl*  nil)
(defparameter *mod-shift* nil)
(defparameter *mod-meta*  nil)
;;;Key handling
(defparameter *key-seq* "")
(defparameter *key-last* 0)
(defparameter *key-start* 0)
(defparameter *key-timeout* 1000)
(defparameter *key-max* 1000)
;;;Widgets
;;string => function
(defparameter *factories* (make-hash-table))
;;int => widget
(defparameter *widgets-lock* (make-recursive-lock "widgets-lock"))
(defparameter *widgets* nil)
;;widget => int
(defparameter *rwidgets-lock* (make-recursive-lock "rwidgets-lock"))
(defparameter *rwidgets* nil)
(defparameter *root* nil)
(defparameter *key-grab* nil)
(defparameter *mouse-grab* nil) ;XXX: *key-grab* can accomplish the same alone
(defparameter *focused-wdg* nil)
;;;Misc
(defparameter *context-menu* nil)
;;;wdgmsg receiver
(defparameter *wdgmsg-receiver* nil)

;;;Resetter
(defun reset-ui ()
  (setf *key-seq* ""
        *widgets* (make-hash-table)
        *rwidgets* (make-hash-table :test #'eq)
        *root* (make-instance 'widget 
                              :size (make-instance 'hnh-utils:vec
                                                   :x hnh-client:*window-width*
                                                   :y hnh-client:*window-height*)
                              :pos (hnh-utils:zero-vector)
                              :parent nil))
  (setf (gethash 0 *widgets*) *root*)
  (setf (gethash *root* *rwidgets*) 0))

;;;Event handling for widgets
(defun set-mods ()
  "Update modifier keys"
  (if (or (= glfw:+press+ (glfw:get-key glfw:+key-lctrl+))
          (= glfw:+press+ (glfw:get-key glfw:+key-rctrl+)))
      (setf *mod-ctrl* t)
      (setf *mod-ctrl* nil))
  (if (or (= glfw:+press+ (glfw:get-key glfw:+key-lshift+))
          (= glfw:+press+ (glfw:get-key glfw:+key-rshift+)))
      (setf *mod-shift* t)
      (setf *mod-shift* nil))
  (if (or (= glfw:+press+ (glfw:get-key glfw:+key-lalt+))
          (= glfw:+press+ (glfw:get-key glfw:+key-ralt+)))
      (setf *mod-meta* t)
      (setf *mod-meta* nil)))

(defun modifier (key)
  "checks if the key is a modifier"
  (case key 
    ((or :lshift :rshift 
         :lctrl :rctrl
         :lalt :ralt)
     t)
    (t nil)))
  
(defun on-key-down (key-sym)
  ;;ignore modifiers
  (set-mods)
  (unless (modifier key-sym)
    (update-seq key-sym)
    (format t "SEQ: ~A ~%" *key-seq*)
    (if *key-grab*
        (process-kseq *key-grab* *key-seq*)
        (process-kseq *root* *key-seq*))))

(defun on-key-up (key-sym)
  ;;ignore modifiers
  (set-mods)
  (unless (modifier key-sym)
    (when *key-grab*
      (key-up *key-grab* key-sym))))

(defun on-char (char)
  (set-mods)
  (when *key-grab*
    (process-char *key-grab* char)))

(defun on-mouse-down (button x y)
  (set-mods)
  (let ((pos (make-instance 'hnh-utils:vec :x x :y y)))
    (if *mouse-grab*
        (mouse-down *mouse-grab* button pos)
        (mouse-down *root* button pos))))

(defun on-mouse-up (button x y)
  (set-mods)
  (let ((pos (make-instance 'hnh-utils:vec :x x :y y)))
    (if *mouse-grab*
        (mouse-up *mouse-grab* button pos)
        (mouse-up *root* button pos))))

(defun on-mouse-move (x y dx dy)
  (set-mods)
  (let ((pos (make-instance 'hnh-utils:vec :x x :y y))
        (dpos (make-instance 'hnh-utils:vec :x dx :y dy)))
    (if *mouse-grab*
        (mouse-move *mouse-grab* pos dpos)
        (mouse-move *root* pos dpos))))

(defun on-mouse-wheel (amt damt x y)
  (set-mods)
  (let ((pos (make-instance 'hnh-utils:vec :x x :y y)))
    (if *mouse-grab*
        (mouse-wheel *mouse-grab* amt damt pos)
        (mouse-wheel *root* amt damt pos))))
  
(defun remove-seq ()
  (setf *key-seq* ""))

(defun update-seq (key)
  ;;check timeouts
  (when (or (> (- (get-internal-real-time) *key-last*) *key-timeout*)
            (> (- (get-internal-real-time) *key-start*) *key-max*))
    (remove-seq))

  ;;add space if needed or start our timer
  (if (stringp *key-seq*) 
      (setf *key-start* (get-internal-real-time))
      (setf *key-seq* (alexandria:symbolicate *key-seq* '| |)))

  ;;check modifiers
  (when *mod-ctrl*
    (setf *key-seq* (alexandria:symbolicate *key-seq* 'C-)))
  (when *mod-shift*
    (setf *key-seq* (alexandria:symbolicate *key-seq* 'S-)))
  (when *mod-meta*
    (setf *key-seq* (alexandria:symbolicate *key-seq* 'M-)))
  ;;add in current key
  (setf *key-seq* (alexandria:symbolicate *key-seq* key))
  (setf *key-last* (get-internal-real-time)))


;;;UI stuff
(defun bind (wdg id)
  (with-recursive-lock-held (*widgets-lock*)
    (with-recursive-lock-held (*rwidgets-lock*)
      (setf (gethash id *widgets*)   wdg)
      (setf (gethash wdg *rwidgets*) id))))

(defun unbind (wdg)
  (with-recursive-lock-held (*widgets-lock*)
    (with-recursive-lock-held (*rwidgets-lock*)
      (let ((id (gethash wdg *rwidgets*)))
        (remhash wdg *rwidgets*)
        (remhash id *widget*)))))
    
(defun new-widget (id type par-id pos &rest args)
  "Creates a new widget based on its type"
  (let ((producer (gethash type *factory*)))
    (let ((pwdg nil))
      (with-recursive-lock-held (*widgets-lock*)
        (setf pwdg (gethash par-id *widgets*)))
      ;;widgets need a parent...
      (when (null pwdg)
        (error (format nil "Null parent widget ~A for ~A" par-id type)))
      (let ((wdg (funcall producer pwdg pos args)))
        (bind wdg id)))))

(defun remove-wdg (wdg)
  "Removes a widget and all of its children from the UI"
  (with-recursive-lock-held (*widgets-lock*)
    (with-recursive-lock-held (*rwidgets-lock*)
      (let ((id (gethash wdg *rwidgets*)))
        (when id
          (remhash id *widgets*)
          (remhash wdg *rwidgets*)))))
  (dolist (child (children wdg))
    (remove-wdg child)))

(defun get-wdg (id)
  "Gets a widget safely based on its ID"
  (let ((wdg nil))
    (with-recursive-lock-held (*widgets-lock*)
      (setf wdg (gethash id *widgets*)))
    wdg))

(defun get-id (wdg)
  "Gets the id of a given widget safely"
  (let ((id -1))
    (with-recursive-lock-held (*rwidgets-lock*)
      (setf id (gethash wdg *rwidgets*)))
    id))

(defun destroy-wdg (wdg)
  "Removes a widget and all its children from the UI"
  (remove-wdg wdg)
  (destroy wdg)
  (unlink wdg))

(defun destroy-id (id)
  "Same as destroy-wdg but based on the widget ID"
  (let ((wdg nil))
    (with-recursive-lock-held (*widgets-lock*)
      (setf wdg (gethash id *widgets*)))
    (when wdg
      (destroy-wdg wdg))))

(defun draw-ui ()
  (when *root*
    (let ((renderer (hnh-gfx:make-renderer 
                     :size (make-instance 'hnh-utils:vec 
                                          :x hnh-client:*window-width*
                                          :y hnh-client:*window-height*))))
      (draw *root* renderer))))

(defun transfer-focus (wdg)
  "Transfers focus from *focused-wdg* to WDG"
  (take-focus *focused-wdg*)
  (give-focus wdg)
  (setf *focused-wdg* wdg))

(defun mod-flags ()
  (logior (if *mod-shift* 1 0)
          (if *mod-ctrl*  2 0)
          (if *mod-meta*  4 0)))

;;;Widget messages
(defun wdgmsg-1 (sender msg &rest args)
  (let ((id (get-id sender)))
    (if (plusp id)
        (when *wdgmsg-receiver*
          (apply *wdgmsg-receiver* id msg args))
        (format t "WDGMSG sender (~A) is not in rwidgets. ~A ~A~%"
                sender
                msg args))))

(defun uimsg-1 (id msg &rest args)
  (let ((wdg (get-wdg id)))
    (if wdg
        (apply #'uimsg wdg (intern msg) args)
        (format t "UIMSG to non-existent widget ~A, ~A, ~A~%" id msg args))))
