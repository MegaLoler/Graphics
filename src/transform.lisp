(in-package :graphics.transform)

;; these abstractions feel messy to me
(defun translate (translation-vector vector &optional reverse)
  "Translate vectors by a translation vector."
  (funcall (if reverse
	       #'vec-subtract
	       #'vec-add)
	   vector translation-vector))

(defun scale (scaling-vector vector &optional reverse)
  "Scale vectors by a scaling vector."
  (funcall (if reverse
	       #'vec-divide
	       #'vec-multiply)
	   vector scaling-vector))

(defun make-transformation (function initial-vector)
  "Make a pair of reciprocal functions that transform vectors using a given transformation function and a given inital vector."
  (lambda (vector &optional reverse)
    (funcall function initial-vector vector reverse)))

(defun make-translation (translation-vector)
  "Make a function that translates vectors by a specific translation vector."
  (make-transformation #'translate translation-vector))

(defun make-scaling (scaling-vector)
  "Make a function that scales vectors by a specific scaling vector."
  (make-transformation #'scale scaling-vector))

(defun transform (transformation vector)
  "Apply a transformation to a vector."
  (funcall transformation vector))

(defun reverse-transform (transformation vector)
  "Apply a reverse transformation to a vector."
  (funcall transformation vector t))

;; and make rotation later too ; basic rotation functions in vector.lisp?
;; and make function composition!! 
