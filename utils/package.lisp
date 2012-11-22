;;;; util/package

(defpackage #:hnh-utils
  (:nickname :hu)
  (:use :cl)
  (:export #:vec2n
           #:make-vec2n
           #:vec-add
           #:vec-sub
           #:vec-mult
           #:vec-div
           #:vec-length
           #:vec-dot
           #:+zero-vector+

           #:color
           #:make-color
           #:+black+
           #:+white+
           #:+red+
           #:+green+
           #:+blue+
           #:+pink+
           #:+magenta+
           #:+cyan+
           #:+yellow+
           #:+orange+
           #:string-to-colorn
           #:string-to-color))
