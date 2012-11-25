;;;; utils/vec2n.lisp

(in-package :hnh-utils)

(defclass vec ()
  ((x :accessor x
      :initarg :x
      :initform 0
      :type number)
   (y :accessor y
      :initarg :y
      :initform 0
      :type number)))

(defun zero-vector ()
  (make-instance 'vec :x 0 :y 0))

(defmacro make-vec (&key (x 0) (y 0))
  "shortcut for makign vecs"
  `(make-instance 'vec :x ,x :y ,y))

(defgeneric vec-add (v1 v2))
(defmethod vec-add ((c1 vec) (c2 vec))
  "Add two coordinates together"
  (make-instance 'vec 
                 :x (+ (x c1) (x c2))
                 :y (+ (y c1) (y c2))))

(defmethod vec-add ((c1 vec) (x number))
  "Add a coordinate to another number (int,float)"
  (make-instance 'vec
                 :x (+ (x c1) x)
                 :y (+ (y c1) x)))

(defgeneric vec-sub (v1 v2))
(defmethod vec-sub ((c1 vec) (c2 vec))
  "Subtract two coordinates together"
  (make-instance 'vec 
                 :x (- (x c1) (x c2))
                 :y (- (y c1) (y c2))))

(defmethod vec-sub ((c1 vec) (x number))
  "Subtract a coordinate to another number (int,float)"
  (make-instance 'vec
                 :x (- (x c1) x)
                 :y (- (y c1) x)))

(defgeneric vec-mult (v1 v2))
(defmethod vec-mult ((c1 vec) (c2 vec))
  "Multiply two coordinates together"
  (make-instance 'vec 
                 :x (* (x c1) (x c2))
                 :y (* (y c1) (y c2))))

(defmethod vec-mult ((c1 vec) (x number))
  "Multiply a coordinate to another number (int,float)"
  (make-instance 'vec
                 :x (* (x c1) x)
                 :y (* (y c1) x)))

(defgeneric vec-div (v1 v2))
(defmethod vec-div ((c1 vec) (c2 vec))
  "Divide two coordinates together"
  (make-instance 'vec 
                 :x (/ (x c1) (x c2))
                 :y (/ (y c1) (y c2))))

(defmethod vec-div ((c1 vec) (x number))
  "Divide a coordinate to another number (int,float)"
  (make-instance 'vec
                 :x (/ (x c1) x)
                 :y (/ (y c1) x)))

(defmethod vec-length ((c1 vec))
  "Returns length of this vector"
  (sqrt (+ (* (x c1) (x c1))
           (* (y c1) (y c1)))))

(defmethod vec-dot ((v1 vec) (v2 vec))
  "Returns the dot product of two vectors"
  (+ (* (x v1) (x v2))
     (* (y v1) (y v2))))


(defun box-intersect (vec vec-size ul ul-size)
  "Checks if VEC is inside the box created by UL+SIZE"
  (if (or (>= (x vec) ;too far right
              (+ (x ul) (x ul-size)))
          (>= (y vec) ;too far down
              (+ (y ul) (y ul-size)))
          (<= (+ (x vec) (x vec-size))
              (x ul)) ;too far left
          (<= (+ (y vec) (y vec-size))
              (y ul))) ;too far up
      nil 
      t))

(defun within (vec size pos)
  "Checks to see if a point X Y is within a box defined by VEC and SIZE"
  (if (and (>= (x pos) (x vec))
           (<= (x pos) (+ (x vec) (x size)))
           (>= (y pos) (y vec))
           (<= (y pos) (+ (y vec) (y size))))
      t
      nil))
