;;;; ui/wdg-def.lisp

(in-package #:hnh-ui)

(defclass widget ()
  ((parent :accessor parent
           :initarg :parent
           :initform nil
           :type  widget)
   (children :accessor children
             :initform nil
             :type list)
   (position :accessor pos
             :initarg :pos
             :initform nil
             :type hnh-utils:vec)
   (size :accessor size
         :initarg :size
         :initform nil
         :type hnh-utils:vec)
   (can-active :accessor cact
               :initform nil
               :type boolean)
   (can-cancel :accessor ccan
               :initform nil
               :type boolean)
   (has-focus :accessor hfocus
              :initform nil
              :type boolean)
   (can-focus :accessor cfocus
              :initform nil
              :type boolean)
   (visible :accessor visible
            :initform t
            :type boolean)
   (cursor :accessor cursor
           :initform nil)))

(defgeneric link (widget)
  (:documentation "Links the given WIDGET to it's parent"))
(defgeneric unlink (widget)
  (:documentation "Unlinks the given WIDGET from it's parent"))
;;XXX: destroy isn't used nor implemented
(defgeneric destroy (widget)
  (:documentation "Destroys the given WIDGET and all of its children"))
(defgeneric raise (widget)
  (:documentation "Raises the given WIDGET to the front of all other children of 
the parent widget"))

;;XXX: set-focusable isn't used nor implemented
(defgeneric set-focusable (widget value)
  (:documentation "Changes the can-focus value to the given VALUE for the WIDGET given
VALUE should be a boolean"))
;;XXX: set-visible isn't used nor implemented
(defgeneric set-visible (widget value)
  (:documentation "Changes the visibility of the given WIDGET based on VALUE
VALUE should be a boolean"))

;;events
(defgeneric give-focus (widget)
  (:documentation "Gives focus to the given WIDGET"))
(defgeneric take-focus (widget)
  (:documentation "Takes focus from the given WIDGET"))
(defgeneric mouse-move (widget pos dpos)
  (:documentation "Invokes the mouse-move event for a given WIDGET along with its POSition
within the WIDGET and deltas DPOS
Return value is a boolean based on the success of the event"))
(defgeneric mouse-down (widget btn pos)
  (:documentation "Invokes the mouse-down event for a given WIDGET along with its POSition
within the WIDGET
Return value is a boolean based on the success of the event"))
(defgeneric mouse-up (widget btn pos)
  (:documentation "Invokes the mouse-up event for a given WIDGET along with its POSition
within the WIDGET
Return value is a boolean based on the success of the event"))
(defgeneric mouse-wheel (widget amt damt pos)
  (:documentation "Invokes the mouse-wheel event for a given WIDGET along with its POSition
within the WIDGET and the location of the wheel, AMT, along with the delta DAMT
Return value is a boolean based on the success of the event"))
(defgeneric process-kseq (widget kseq)
  (:documentation "Invokes the process-kseq event for a given WIDGET along with the KeySEQ
Return value is a boolean based on the success of the event"))
(defgeneric process-char (widget char)
  (:documentation "Invokes the process-char event for a given WIDGET along with the CHAR
Unlike process-kseq, process-char is mainly used for textual import for textboxes, etc
Return value is a boolean based on the success of the event"))
;;XXX: key-down should be replaced fully by process-kseq
(defgeneric key-down (widget key)
  (:documentation "Invokes the key-down event for a given WIDGET along with the KEY
Return value is a boolean based on the success of the event"))
;;XXX: key-up should be replaced fully by process-kseq
(defgeneric key-up (widget key)
  (:documentation "Invokes the key-up event for a given WIDGET along with the KEY
Return value is a boolean based on the success of the event"))
(defgeneric draw (widget renderer)
  (:documentation "Draws the given WIDGET within its RENDERER space"))
(defgeneric tooltip (widget pos)
  (:documentation "Attempts to get a tooltip from the given WIDGET based on POSition within
it
Return value will be nil if there's no tooltip"))
;;messaging between
(defgeneric wdgmsg (widget sender msg &rest args)
  (:documentation "Sends a MESsage from the SENDER widget to parent WIDGETs along with more
ARGumentS if needed")) 
(defgeneric uimsg (widget msg &rest args)
  (:documentation "Sends a MESsage from the ui to children WIDGETs along with more ARGumentS
if needed"))
;;lookups
;;XXX: has-parent isn't used nor implemented
(defgeneric has-parent (widget parent)
  (:documentation "Finds out if the given WIDGET has PARENT as a parent widget"))
