;;;; ui/widget.lisp
;;;; Purpose: base for all gui

(in-package :hnh-ui)

(defclass widget ()
  ((parent :accessor widget-parent
           :initarg :parent
           :initform nil
           :type widget)
   (children :accessor widget-children
             :initarg :children
             :initform ()
             :type cons)
   (ui :accessor widget-ui
       :initarg :ui
       :initform nil)
   (pos :accessor widget-pos
        :initarg :pos
        :initform nil
        :type hnh-utils:vec2n)
   (size :accessor widget-size
         :initarg :size
         :initform nil
         :type hnh-utils:vec2n)
   
