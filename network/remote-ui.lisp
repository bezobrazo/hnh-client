;;;; network/remote-ui.lisp
(in-package #:hnh-network)
;;;Relevant constants
(defconstant +rmsg-newwdg+ 0)
(defconstant +rmsg-wdgmsg+ 1)
(defconstant +rmsg-dstwdg+ 2)
;;;Message queue
(defparameter *rmsg-queue* nil)
(defparameter *rmsg-queue-lock* (make-recursive-lock "rmsg-queue-lock"))
(defparameter *rnd-lock* (make-lock "useless-lock"))
(defparameter *rmsg-cond* (make-condition-variable :name "rmsg condition"))


(defun send-wdgmsg (id msg-name &rest args)
  (let ((msg (make-message :type +rmsg-wdgmsg+)))
    (message-push-int msg id 2)
    (message-push-str msg msg-name)
    (message-push-list args)
    (queue-msg msg)))

(defun run-remote-ui ()
  (setf hnh-ui:*wdgmsg-receiver* #'send-wdgmsg)
  (loop until (eq '|dead| *state*)
     do (let ((msg nil))
          (with-recursive-lock-held (*rmsg-queue-lock*)
            (setf msg (pop *rmsg-queue*)))
          (when msg
            (case (message-type msg)
              (+rmsg-newwdg+ (let ((id   (message-pop-uint msg 2))
                                   (name (message-pop-str msg))
                                   (pos  (message-pop-vec msg))
                                   (pid  (message-pop-uint msg 2))
                                   (args (message-pop-list msg)))
                               (apply #'hnh-ui:new-widget id name pid pos args)))
              (+rmsg-wdgmsg+ (let ((id   (message-pop-uint msg 2))
                                   (name (message-pop-str msg)))
                               (hnh-ui:uimsg-1 id name (message-pop-list msg)))) 
              (+rmsg-dstwdg+ (let ((id (message-pop-uint msg 2)))
                               (hnh-ui:destroy-id id)))
              (t (format t "Unhandled RMSG ~A, ~A~%" 
                         (message-type msg)
                         (message-blob msg))))))
       (with-lock-held (*rnd-lock*)
         (condition-wait *rmsg-cond* *rnd-lock*))))
