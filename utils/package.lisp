;;;; util/package

(defpackage #:hnh-utils
  (:use :cl)
  (:export #:vec
           #:x #:y
           
           #:make-vec
           #:copy-vec
           #:vec-add
           #:vec-sub
           #:vec-mult
           #:vec-div
           #:vec-length
           #:vec-dot
           #:zero-vector
           #:box-intersect
           #:within))
