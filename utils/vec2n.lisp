;;;; utils/vec2n.lisp

(in-package :hnh-utils)

(defclass vec2n ()
  ((x :accessor vec2n-x
      :initarg :x
      :initform 0
      :type number)
   (y :accessor vec2n-y
      :initarg :y
      :initform 0
      :type number)))

(defconstant +zero-vector+
  (make-instance 'vec2n :x 0 :y 0))

(defun zero-vector ()
  (make-instance 'vec2n :x 0 :y 0))

(defmacro make-vec2n (&key (x 0) (y 0))
  "shortcut for makign vec2ns"
  `(make-instance 'vec2n :x ,x :y ,y))

(defgeneric vec-add (v1 v2))
(defmethod vec-add ((c1 vec2n) (c2 vec2n))
  "Add two coordinates together"
  (make-instance 'vec2n 
                 :x (+ (vec2n-x c1) (vec2n-x c2))
                 :y (+ (vec2n-y c1) (vec2n-y c2))))

(defmethod vec-add ((c1 vec2n) (x number))
  "Add a coordinate to another number (int,float)"
  (make-instance 'vec2n
                 :x (+ (vec2n-x c1) x)
                 :y (+ (vec2n-y c1) x)))

(defgeneric vec-sub (v1 v2))
(defmethod vec-sub ((c1 vec2n) (c2 vec2n))
  "Subtract two coordinates together"
  (make-instance 'vec2n 
                 :x (- (vec2n-x c1) (vec2n-x c2))
                 :y (- (vec2n-y c1) (vec2n-y c2))))

(defmethod vec-sub ((c1 vec2n) (x number))
  "Subtract a coordinate to another number (int,float)"
  (make-instance 'vec2n
                 :x (- (vec2n-x c1) x)
                 :y (- (vec2n-y c1) x)))

(defgeneric vec-mult (v1 v2))
(defmethod vec-mult ((c1 vec2n) (c2 vec2n))
  "Multiply two coordinates together"
  (make-instance 'vec2n 
                 :x (* (vec2n-x c1) (vec2n-x c2))
                 :y (* (vec2n-y c1) (vec2n-y c2))))

(defmethod vec-mult ((c1 vec2n) (x number))
  "Multiply a coordinate to another number (int,float)"
  (make-instance 'vec2n
                 :x (* (vec2n-x c1) x)
                 :y (* (vec2n-y c1) x)))

(defgeneric vec-div (v1 v2))
(defmethod vec-div ((c1 vec2n) (c2 vec2n))
  "Divide two coordinates together"
  (make-instance 'vec2n 
                 :x (/ (vec2n-x c1) (vec2n-x c2))
                 :y (/ (vec2n-y c1) (vec2n-y c2))))

(defmethod vec-div ((c1 vec2n) (x number))
  "Divide a coordinate to another number (int,float)"
  (make-instance 'vec2n
                 :x (/ (vec2n-x c1) x)
                 :y (/ (vec2n-y c1) x)))

(defmethod vec-length ((c1 vec2n))
  "Returns length of this vector"
  (sqrt (+ (* (vec2n-x c1) (vec2n-x c1))
           (* (vec2n-y c1) (vec2n-y c1)))))

(defmethod vec-dot ((v1 vec2n) (v2 vec2n))
  "Returns the dot product of two vectors"
  (+ (* (vec2n-x v1) (vec2n-x v2))
     (* (vec2n-y v1) (vec2n-y v2))))


(defun box-intersect (vec vec-size ul ul-size)
  "Checks if VEC is inside the box created by UL+SIZE"
  (if (or (>= (vec2n-x vec) ;too far right
              (+ (vec2n-x ul) (vec2n-x ul-size)))
          (>= (vec2n-y vec) ;too far down
              (+ (vec2n-y ul) (vec2n-y ul-size)))
          (<= (+ (vec2n-x vec) (vec2n-x vec-size))
              (vec2n-x ul)) ;too far left
          (<= (+ (vec2n-y vec) (vec2n-y vec-size))
              (vec2n-y ul))) ;too far up
      nil 
      t))
