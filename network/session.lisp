;;;; network/session.lisp
(in-package #:hnh-network)

;;;MSG constants
(defconstant +pver+        2)

(defconstant +msg-sess+    0)
(defconstant +msg-rel+     1)
(defconstant +msg-act+     2)
(defconstant +msg-beat+    3)
(defconstant +msg-mapreq+  4)
(defconstant +msg-mapdata+ 5)
(defconstant +msg-objdata+ 6)
(defconstant +msg-objack+  7)
(defconstant +msg-close+   8)
;;;ObjData types
(defconstant +od-rem+      0)
(defconstant +od-move+     1)
(defconstant +od-res+      2)
(defconstant +od-linbeg+   3)
(defconstant +od-linstep+  4)
(defconstant +od-speech+   5)
(defconstant +od-layers+   6)
(defconstant +od-drawoff+  7)
(defconstant +od-lumin+    8)
(defconstant +od-avatar+   9)
(defconstant +od-follow+   10)
(defconstant +od-homing+   11)
(defconstant +od-overlay+  12)
(defconstant +od-auth+     13)
(defconstant +od-health+   14)
(defconstant +od-buddy+    15)
(defconstant +od-end+      255)
;;;Session states
(defconstant +sesserr-auth+ 1)
(defconstant +sesserr-busy+ 2)
(defconstant +sesserr-conn+ 3)
(defconstant +sesserr-pver+ 4)
(defconstant +sesserr-expr+ 5)

;;;Server socket
(defparameter *sock* nil)
(defparameter *server* "")
(defparameter *server-port* 1870)

;;;Player details
(defparameter *user-name* "")
(defparameter *cookie* nil)

;;;Message queues and locks
(defparameter *outbound-lock* (make-recursive-lock "outbound-IO-lock"))
(defparameter *outbound-queue* nil)
(defparameter *outbound-len* 0)
(defparameter *waiting-lock* (make-recursive-lock "waiting-lock"))
(defparameter *waiting-map* (make-hash-table)) ;int=>msg
(defparameter *objacks-lock* (make-recursive-lock "objacts-lock"))
(defparameter *objacks-map* (make-hash-table)) ;int=>objack

;;;States
(defparameter *state* nil)
(defparameter *conn-fail* nil)

;;;Misc
(defparameter *tseq* 0)
(defparameter *rseq* 0)
(defparameter *ack-seq* 0)
(defparameter *act-lock* (make-recursive-lock "act-lock"))
(defparameter *act-time* -1)

(defparameter *milli-to-seconds* 1000)
(defparameter *beat-buffer* (make-array 1 
                                        :element-type '(unsigned-byte 8)
                                        :initial-element +msg-beat+))

;;;Objact structure
(defstruct objact
  (id 0 :type fixnum)
  (frame 0 :type fixnum)
  (recv 0 :type fixnum)
  (sent  0 :type fixnum))

;;; Session Writer stuff
(defun run-swriter ()
  (let ((to 0)
        (last 0)
        (retries 0))
    (loop do
         (let ((now (get-internal-real-time)))
           (case *state*
             ('|conn|
              (when (> (- now last) 2000)
                (incf retries)
                (when (> retries 5)
                  (setf *conn-fail* +sesserr-conn+)
                  (return-from run-swriter))
                (let ((msg (make-message :type +msg-sess+)))
                  (message-push-int msg 1 2)
                  (message-push-str msg "Haven")
                  (message-push-int msg +pver+ 2)
                  (message-push-str msg *user-name*)
                  (message-push-bytes msg *cookie* 0 (length *cookie*))
                  (send-msg msg)
                  (setf last now)))
              (sleep (/ 100 *milli-to-seconds*)))
             (t
              (setf to 5000)
              (with-recursive-lock-held (*outbound-lock*)
                (when (plusp *outbound-len*)
                  (setf to 60)))
              (with-recursive-lock-held (*objacts-lock*)
                (when (and (plusp (length *objacts-map*))
                           (> to 120))
                  (setf to 200)))
              (with-recursive-lock-held (*act-lock*)
                (when (plusp *act-time*)
                  (setf to (- (+ *act-time* *act-thresh*) now)))
                (when (plusp to) ;;XXX: sleep needs to be a condition wait
                  (sleep (/ to *milli-to-seconds*))))
              (setf now (get-internal-real-time))
              (let ((beat t))
                (with-recursive-lock-held (*outbound-lock*)
                  (when (plusp *outbound-len*)
                    (setf beat nil))
                  (dolist (msg *outbound-queue*)
                    (let ((txtime 0))
                      (cond 
                        ((= (message-retx msg) 1)
                         (setf txtime 80))
                        ((< (message-retx msg) 4)
                         (setf txtime 200))
                        ((< (message-retx msg) 10)
                         (setf txtime 620))
                        (t (setf txtime 2000)))
                      (when (> (- no (message-last msg)) txtime)
                        (setf (message-last msg) now)
                        (incf (message-retx msg))
                        (let ((nmsg (make-message :type +msg-rel+)))
                          (message-push-int nmsg (message-seq msg) 2)
                          (message-push-int nmsg (message-type msg) 1)
                          (message-push-bytes nmsg
                                              (message-blob msg)
                                              0
                                              (length (message-blob msg)))
                          (send-msg msg))))))
                (with-recursive-lock-held (*objacts-lock*)
                  (let ((msg nil))
                    (with-hash-table-iterator (itr *objacts-map*)
                      (loop 
                           (multiple-value-bind (entry-p key value)
                               (itr)
                             (if entry-p
                                 (let ((send nil)
                                     (del nil))
                                   (cond 
                                     ((> (- now (objact-sent value)) 200)
                                      (setf send t))
                                     ((> (- now (objact-recv value)) 120)
                                      (setf send t
                                            del t)))
                                   (when send
                                     (when (null msg)
                                       (setf msg (make-message 
                                                  :type +msg-objack+)))
                                     (message-push-int msg
                                                       (objact-id value)
                                                       4)
                                     (message-push-int msg
                                                       (objact-frame value)
                                                       4)
                                     (setf (objact-sent value) now))
                                   (when del
                                     (remhash key *objacts-map*))
                                 (return))))))
                    (when msg
                      (send-msg msg)
                      (setf beat nil))))
                ;;XXX: possible lock needed
                (when (and (plusp *act-time*)
                         (>= (- now *act-time*) *act-thresh*))
                  (let ((msg (make-message :type +msg-ack+)))
                    (message-push-int msg *ack-seq* 2)
                    (send-msg msg)
                    (setf *act-time* -1
                          beat nil)))
                (when beat
                  (when (> (- now last) 5000)
                    (send-msg *beat-buffer*)
                    (setf last now))))))))))


;;;Session stuff

(defun send-ack (seq)
  ;;XXX: possible lock needed
  (when (< *act-time* 0)
    (setf *act-time* (get-internal-real-time)))
  (setf *act-seq* seq))

(defun queue-msg (msg)
  "Adds a message to the outbound queue"
  (setf (message-seq msg) *tseq*)
  (setf *tseq* (mod (1+ *tseq*) 65536))
  (with-recursive-lock-held (*outbound-lock*)
    (push msg *outbound-queue*)
    (incf *outbound-len*))
  ;;XXX: notify sworker
  )

(defun send-msg (msg) 
  "Sends a message to the server"
  (let ((buf (make-u8array (1+ (length (message-blob msg))))))
    (setf (aref buf 0) (message-type msg))
    (array-copy (message-blob msg) 0 buf 1 (length (message-blob msg)))
    (send-msg-1 buf)))

(defun send-msg-1 (buffer)
  "Sends a byte buffer to the server"
  (usocket:socket-send *sock* buffer (length buffer)
                       :host *server*
                       :port *server-port*))
