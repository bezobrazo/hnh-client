;;;; network/auth.lisp
;;;; Purpose: Handles authentication of user into hnh

(in-package :hnh-network)

;;;HNH server: moltke.seatribe.se
;;;HNH port  : 1871
(defconstant +auth-port+ 1871)
(defconstant +auth-server+ "moltke.seatribe.se")

;;;Message types relevant to the auth-server process
(defconstant +cmd-usr+ 1)
(defconstant +cmd-passwd+ 2)
(defconstant +cmd-gettoken+ 3)
(defconstant +cmd-usetoken+ 4)


(defun auth-create-socket (host port)
  "Creates the ssl-stream & socket needed for auth server"
  ;;make tcp socket, create ssl-stream, return both
  (let ((socket (usocket:socket-connect host port :protocol :stream)))
    (values (cl+ssl:make-ssl-client-stream (usocket:socket-stream socket)
                                           :certificate (get-pathname "authsrv.crt"))
            socket)))

(defun auth-send (ssl-out-stream msg)
  "Sends a message to the auth server"
  ;;auth-server only supports up to 255 bytes due to length being a byte big
  (when (> (length (message-blob msg)) 255)
    (error "Message is too long to be sent to Auth server..."))
  ;;make full-buffer
  (let ((buf (make-u8array (+ (length (message-blob msg)) 2))))
    ;;add in type and length and blob
    (setf (aref buf 0) (message-type msg))
    (setf (aref buf 1) (aref (int->uba (length (message-blob msg)) 1) 0))
    (array-copy (message-blob msg) 0 buf 2 (length (message-blob msg)))
    ;;write-to-stream and force it
    (write-sequence buf ssl-out-stream)
    (force-output ssl-out-stream)))

(defun auth-rec (ssl-in-stream)
  "Recieves a message from the auth server"
  (let ((header (make-u8array 2)))
    ;;read in header from stream
    (read-all ssl-in-stream header)
    (let ((blob (make-u8array (aref header 1))))
      ;;read in blob based on header length
      (read-all ssl-in-stream blob)
      ;;transform into message
      (make-message :type (aref header 0)
                    :blob blob))))
      
(defun auth-bind-user (stream username)
  "Binds auth-server to this username"
  ;;create cmd-usr message type
  (let ((umsg (make-message :type +cmd-usr+)))
    ;;add in our username
    (message-push-str-1 umsg username)
    ;;send it
    (auth-send stream umsg)
    ;;get result
    (let ((rmsg (auth-rec stream)))
      ;;returned type should be 0, otherwise some type of error
      (unless (zerop (message-type rmsg))
        (error (format nil "Error: message type no recognized [~A] when binding username"
                       (message-type rmsg)))))))

(defun auth-try-passwd (stream password)
  "Generates cookie for player to log into hnh"
  ;;create password message type
  (let ((umsg (make-message :type +cmd-passwd+)))
    ;;add in password in sha256 hash form
    (message-push-bytes-1 umsg (ironclad:digest-sequence 
                                :sha256
                                (babel:string-to-octets password :encoding :utf-8)))
    ;;send it
    (auth-send stream umsg)
    ;;get result
    (let ((rmsg (auth-rec stream)))
      ;;result type of 0 refers to success
      (if (zerop (message-type rmsg))
          ;;blob should be a cookie that we need for session
          (message-blob rmsg)
          ;;otherwise nil
          nil))))

(defun auth-validate (host port username password)
  "Attempts to log in user, returns cookie if successful otherwise nil"
  (let ((cookie nil))
    ;;create socket and ssl-stream
    (multiple-value-bind (stream usock)
        (auth-create-socket host port)
      ;;bind username to stream
      (auth-bind-user stream username)
      ;;attempt login with given password for cookie
      (setf cookie (auth-try-passwd stream password))
      ;;clean up
      (close stream)
      (usocket:socket-close usock))
    ;;return cookie
    cookie))
