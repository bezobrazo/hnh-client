;;;; ui/widget.lisp

(in-package #:hnh-ui)

(defun produce-widget (parent pos &rest args)
  (make-instance 'widget :parent parent
                 :pos pos
                 :size (car args)))
(setf (gethash "cnt" *factories*) #'produce-widget)

;;;Widget implementations

(defmethod link ((wdg widget))
  (when (parent wdg)
    (setf (children (parent wdg))
          (push wdg (children (parent wdg))))))

(defmethod unlink ((wdg widget))
  (when (parent wdg)
    (setf (children (parent wdg))
          (remove wdg (children (parent wdg))))))

(defmethod raise ((wdg widget))
  (when (parent wdg)
    (setf (children (parent wdg))
          (remove wdg (children (parent wdg))))
    (setf (children (parent wdg))
          (push wdg (children (parent wdg))))))

;;; Event implementations from widget simply invoke their children

(defmethod process-kseq ((wdg widget) kseq)
  (case kseq
    ('|ENTER| (when (cact wdg)
                (wdgmsg wdg wdg "activate")
                (return-from process-kseq t)))
    ('|ESC| (when (ccan wdg)
              (wdgmsg wdg wdg "cancel")
              (return-from process-kseq t)))
    (t 
     (dolist (child (children wdg))
       (when (and (visible child)
                  (process-kseq child kseq))
         (return-from process-kseq t)))))
  nil)
  
(defmethod process-char ((wdg widget) char)
  (dolist (child (children wdg))
    (when (and (visible child)
               (process-char child char))
      (return-from process-char t)))
  nil)

(defmethod mouse-move ((wdg widget) pos dpos)
  (dolist (child (children wdg))
    (when (and (visible (child))
               (hnh-utils:within (pos child)
                                 (size child)
                                 pos)
               (mouse-move child 
                           (hnh-utils:vec-sub (pos child) pos)
                           dpos))
      (return-from mouse-move t)))
  nil)

(defmethod mouse-down ((wdg widget) btn pos)
  (dolist (child (children wdg))
    (when (and (visible (child))
               (hnh-utils:within (pos child)
                                 (size child)
                                 pos)
               (mouse-down child 
                           btn
                           (hnh-utils:vec-sub (pos child) pos)))
      (return-from mouse-down t)))
  nil)
 
(defmethod mouse-up ((wdg widget) btn pos)
  (dolist (child (children wdg))
    (when (and (visible (child))
               (hnh-utils:within (pos child)
                                 (size child)
                                 pos)
               (mouse-up child 
                         btn
                         (hnh-utils:vec-sub (pos child) pos)))
      (return-from mouse-up t)))
  nil)

(defmethod mouse-wheel ((wdg widget) amt damt pos)
  (dolist (child (children wdg))
    (when (and (visible (child))
               (hnh-utils:within (pos child)
                                 (size child)
                                 pos)
               (mouse-wheel child 
                            amt damt
                            (hnh-utils:vec-sub (pos child) pos)))
      (return-from mouse-wheel t)))
  nil)

(defmethod key-down ((wdg widget) key)
  (dolist (child (children wdg))
    (when (and (visible child)
               (key-down child key))
      (return-from key-down t)))
  nil)

(defmethod key-up ((wdg widget) key)
  (dolist (child (children wdg))
    (when (and (visible child)
               (key-up child key))
      (return-from key-up t)))
  nil)

(defmethod draw ((wdg widget) rend)
  (dolist (child (children wdg))
    (when (visible child)
      (let ((nrend (hnh-gfx:upd-render rend (pos child) (size child))))
        (draw child nrend)))))

(defmethod tooltip ((wdg widget) pos)
  (dolist (child (children wdg))
    (when (and (visible child)
               (hnh-utils:within (pos child)
                                 (size child)
                                 pos))
      (let ((tt (tooltip child (hnh-utils:vec-sub (pos child) pos))))
        (when tt
          (return-from tooltip tt)))))
  nil)


;;; Widget implementation of (UIMSG) or (WDGMSG) which adds the message
;;; to a queue to be sent to the server
(defmethod wdgmsg ((wdg widget) (sender widget) msg &rest args)
  (if (parent wdg)
      (apply #'wdgmsg (parent wdg) sender msg args)
      (wdgmsg-1 sender msg args)))

(defun find-next-focus (wdg)
  "Finds the next focusable widget from WDGs children and gives it focus"
  (dolist (child (children wdg))
    (when (cfocus child)
      (transfer-focus child)
      (return-from find-next-focus))))
    

(defun change-focus (wdg-id)
  "Changes focus to the widget with the given WDG-ID"
  (let ((wdg (get-wdg id)))
    (when (cfocus wdg)
      (transfer-focus wdg))))
  
;;XXX: fix tabfocus and curs
(defmethod uimsg ((wdg widget) msg &rest args)
  (format t "MSG: ~A, ~A~%" msg args)
  (case msg
    (|tabfocus|  (find-next-focus wdg))
    (|act|       (setf (cact wdg)   (/= (car args) 0)))
    (|cancel|    (setf (ccan wdg)   (/= (car args) 0)))
    (|autofocus| (setf (cfocus wdg) (/= (car args) 0))))
    (|focus|     (change-focus (car args)))
    (|curs| t)
    (t (format t "Unhandled widget message ~A~%" msg)))
