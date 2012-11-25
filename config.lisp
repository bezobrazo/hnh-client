;;;; config.lisp

(in-package #:hnh-client)

;;;global variables and their defaults
(defparameter *window-width*  800)
(defparameter *window-height* 600)


(defun parse-args (args)
  (do ((arg args (cdr arg)))
      ((null arg))
    (cond ((string= "-w" (car arg))
           (setf arg (cdr arg))
           (setf *window-width* (parse-integer (car arg))))
          ((string= "-h" (car arg))
           (setf arg (cdr arg))
           (setf *window-height* (parse-integer (car arg)))))))
