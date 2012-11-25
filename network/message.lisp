;;;; network/message.lisp
;;;; Purpose: makes life easier

(in-package :hnh-network)

(defconstant +t-end+ 0)
(defconstant +t-int+ 1)
(defconstant +t-str+ 2)
(defconstant +t-coord+ 3)
(defconstant +t-color+ 6)

(defstruct message
  (off 0 :type fixnum)
  (type -1 :type fixnum)
  (blob #() :type vector)
  ;;session stuff
  (last 0 :type fixnum)
  (seq 0 :type fixnum)
  (retx 0 :type fixnum))


(defun message-push-bytes (msg src off len)
  "Adds a series of bytes given by [off,len] to the blob"
  (let ((nbuf (make-u8array (+ (length (message-blob msg)) len))))
    (array-copy (message-blob msg) 0 nbuf 0 (length (message-blob msg)))
    (array-copy src off nbuf (length (message-blob msg)) len)
    (setf (message-blob msg) nbuf)))

(defun message-push-bytes-1 (msg src)
  "Adds a series of bytes to blob"
  (message-push-bytes msg src 0 (length src)))

(defun message-push-int (msg int bytes)
  "Adds an integer of size bytes to blob"
  (message-push-bytes-1 msg 
                       (int->uba int bytes)))
(defun message-push-str (msg str)
  "Adds a string to the blob, \0 terminated"
  (message-push-bytes-1 msg
                       (str->uba str)))

(defun message-push-str-1 (msg str)
  "Adds a string to the blob"
  (message-push-bytes-1 msg
                       (str->uba-1 str)))

(defun message-eom (msg)
  "Checks if we've hit the end of our message"
  (>= (message-off msg)
      (length (message-blob msg))))

(defun message-pop-uint (msg bytes)
  "Returns an unsigned integer from the message"
  (let ((int (uba->uint (message-blob msg)
                        (message-off msg)
                        bytes)))
    (incf (message-off msg) bytes)
    int))
  
(defun message-pop-sint (msg bytes)
  "Returns a signed integer from the message"
  (let ((si (message-pop-uint msg bytes)))
    (when (> si (1- (ash 1 (1- (* bytes 8)))))
      (setf si (- si (ash 1 (* bytes 8)))))
    si))

(defun message-pop-str (msg)
  "Returns the next string within the message"
  ;;find \0
  (let ((end (loop for i from (message-off msg)
                  to (1- (length (message-blob msg)))
                  while (not (zerop (aref (message-blob msg) i)))
                  finally (return i))))
    (uba->str (message-blob msg) (message-off msg) end)))

(defun message-pop-vec (msg)
  "Returns the next vector within the message"
  (make-instance 'hnh-utils:vec 
                 :x (message-pop-sint msg 4)
                 :y (message-pop-sint msg 4)))

(defun message-pop-color (msg)
  "Returns the next color within the message"
  (imago:make-color (message-pop-uint msg 1)
                    (message-pop-uint msg 1)
                    (message-pop-uint msg 1)
                    (message-pop-uint msg 1)))

(defun message-pop-list (msg)
  "Returns the next list of things within the messge"
  (let ((lst nil))
    (loop do
         (when (>= (message-off msg)
                   (length (message-blob msg)))
           (return-from message-pop-list (nreverse lst)))
         (let ((type (message-pop-uint msg 1)))
           (cond 
             ((= type +t-end+)
              (return-from message-pop-list (nreverse lst)))
             ((= type +t-int+)
              (setf lst (push (message-pop-sint msg 4) lst)))
             ((= type +t-str+)
              (setf lst (push (message-pop-str msg) lst)))
             ((= type +t-coord+)
              (setf lst (push (message-pop-vec msg) lst)))
             ((= type +t-color+)
              (setf lst (push (message-pop-color msg) lst))))))))
