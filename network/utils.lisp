;;;; network/utils.lisp
;;;; Purpose: general purpose helper functions for Auth and Session

(in-package :hnh-network)

(defun read-all (stream buffer)
  "In in n bytes given by length of buffer through the tcp stream"
  (let ((bt 0))
    ;;loop for length of our buffer
    (do ((i 0 (+ i bt)))
        ((>= i (length buffer)))
      ;;make sure the stream has data waiting for us
      (unless (listen stream)
        (error "premature end of input @ read-all"))
      ;;get some seqeunce of bytes, record how much we were able to read and repeat till done
      (setf bt (read-sequence buffer stream :start i :end (length buffer))))))

(defun get-pathname (file)
  "Forms the current pathname with a file we need within it"
  (format nil "~A/~A" *default-pathname-defaults* file))

(defun str->uba (str)
  "Converts a string to an unsigned byte array, \0 terminated"
  (concatenate 'vector
               (babel:string-to-octets str :encoding :utf-8)
               #(0)))

(defun str->uba-1 (str)
  "Converts a string to an unsigned byte array"
  (babel:string-to-octets str :encoding :utf-8))

(defun uba->str (ubarr off end)
  "Converts a series of unsigned bytes to String, utf-8"
  (babel:octets-to-string ubarr 
                          :start off
                          :end end
                          :encoding :utf-8))

(defun uba->uint-1 (ubarr)
  "Makes an array of unsigned bytes into unsigned integer"
  (reduce #'(lambda (l r)
              (+ (ash l 8) r))
          (nreverse ubarr)))

(defun uba->uint (ubarr off len)
  "Makes an array of unsigned bytes from [off,off+len)into an unsigned integer"
  (let ((int 0))
    (do ((i 0 (1+ i)))
        ((= i len))
      (setf int (+ int (ash (aref ubarr (+ i off))
                            (* i 8)))))
    int))

(defun int->uba (int bytes)
  "Takes a signed/unsigned integer and transforms it into a series of unsigned bytes given by the BYTES length"
  (let ((ubarr (make-array bytes
                           :element-type '(unsigned-byte 8)
                           :initial-element 0)))
    (dotimes (i bytes)
      (setf (aref ubarr i)
            (logand int #xff))
      (setf int (ash int -8)))
    ubarr))

(defun array-copy (a1 a1off a2 a2off finish)
  "Copies the contents of a1 from [a1off,a1off+finish) into a2"
  (do ((stop (+ a1off finish)))
      ((= a1off stop))
    (setf (aref a2 a2off)
          (aref a1 a1off))
    (incf a1off)
    (incf a2off)))

(defmacro make-u8array (len)
  "shortcut for making u8 arrays"
  `(make-array ,len 
               :initial-element 0
               :element-type '(unsigned-byte 8)))
