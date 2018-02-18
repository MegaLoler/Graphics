(in-package :graphics.geometry)

(defun csg-n-sphere (n vector)
  "Is location inside a unit n-sphere at origin?"
  (< (vec-distance (subseq vector 0 n)) 1))

(defun make-csg-n-sphere (n)
  "Make a csg form function that represents an n-sphere."
  (lambda (vector)
    (funcall #'csg-n-sphere n vector)))

(defun make-csg-circle ()
  "Make a csg form function that represents a 2d circle."
  (make-csg-n-sphere 2))

(defun make-csg-sphere ()
  "Make a csg form function that represents a 3d sphere."
  (make-csg-n-sphere 3))

(defun csg-transform (form-function transformation-function)
  "Transform a csg form function with a transformation function."
  (lambda (vector)
    (funcall form-function (reverse-transform transformation-function vector))))

(defun csg-operation (operation form-functions)
  "Make a csg form function that is the result of performing an operation on given form functions."
  (lambda (vector)
    (apply operation (mapcar (lambda (form-function)
				 (funcall form-function vector))
			       form-functions))))

(defun csg-union (&rest form-functions)
  "Make a csg form function that represents the union of the forms represented by the given form functions."
  (csg-operation #'or-f form-functions))

(defun csg-intersection (&rest form-functions)
  "Make a csg form function that represents the intersection of the forms represented by the given form functions."
  (csg-operation #'and-f form-functions))

(defun csg-negation (form-function)
  "Make a csg form function that represents the negative space of the form represented by the given form function."
  (csg-operation #'not (list form-function)))

(defun csg-difference (initial-form-function form-function)
  "Subtract the second form from the initial form."
  (csg-intersection initial-form-function (csg-negation form-function)))

(defun csg-color-fg (form-function color)
  "Return a function that returns a color in place of T."
  (lambda (vector)
    (let ((result (funcall form-function vector)))
      (if (equal t result) color result))))

(defun csg-color-bg (form-function color)
  "Return a function that returns a color in place of nil."
  (lambda (vector)
    (let ((result (funcall form-function vector)))
      (if result result color))))
