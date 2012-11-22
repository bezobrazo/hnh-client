;;;; utils/color.lisp

(in-package #:hnh-utils)

(defstruct color
  (red   255 :type number)
  (green 255 :type number)
  (blue  255 :type number)
  (alpha 255 :type number))

;;;Some commonly used color strings
(defconstant +black+   "0;0;0;255")
(defconstant +white+   "255;255;255;255")
(defconstant +red+     "255;0;0;255")
(defconstant +green+   "0;255;0;255")
(defconstant +blue+    "0;0;255;255")
(defconstant +pink+    "255;192;203;255")
(defconstant +magenta+ "255;0;255;255") 
(defconstant +cyan+    "0;255;255;255")
(defconstant +yellow+  "255;255;0;255")
(defconstant +orange+  "255;127;0;255")
 
(defun string-to-colorn (color str)
  "modifies the existing color based on the color string
string format: red;green;blue;alpha where any are
optional" 
  (let ((ind 0))
    ;;parse string
    (loop until (not (find #\; str)) do
         (let* ((pos (position #\; str))
                (num (parse-integer (subseq str 0 pos))))
           (setf str (subseq str (1+ pos)))
           (case ind
             (0 (setf (color-red   color) num))
             (1 (setf (color-green color) num))
             (2 (setf (color-blue  color) num))
             (3 (setf (color-alpha color) num)))
           (incf ind)))
    ;;catch last color
    (when (> (length str) 0)
      (let ((num (parse-integer str)))
        (case ind
          (0 (setf (color-red   color) num))
          (1 (setf (color-green color) num))
          (2 (setf (color-blue  color) num))
          (3 (setf (color-alpha color) num))))))
    color)

(defun string-to-color (str)
  "converts a color string into a color
string format: red;green;blue;alpha where any are
optional"
  (let ((color (make-color)))
    (string-to-colorn color str)))
