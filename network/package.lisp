;;;; network/package.lisp

(defpackage #:hnh-network
  (:use #:cl #:bordeaux-threads)
  (:export
   #:send-wdgmsg))
