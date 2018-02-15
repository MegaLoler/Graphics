;; todo:
;;   make literals for percentages, degrees, turns, and radians

(in-package :graphics.angle)

;; generic angle class
(defclass angle () ())

;; angle as radians
(defclass radians (angle)
  ((radians :initform 0
	    :initarg :radians
	    :accessor radians
	    :type real)))

;; angle as degrees
(defclass degrees (angle)
  ((degrees :initform 0
	    :initarg :degrees
	    :accessor degrees
	    :type real)))

;; angle as turns
(defclass turns (angle)
  ((turns :initform 0
	  :initarg :turns
	  :accessor turns
	  :type real)))

;; make the make-radians constructor
(defconstructor radians (radians))

;; make the make-degrees constructor
(defconstructor degrees (degrees))

;; make the make-turns constructor
(defconstructor turns (turns))

;; general constructor make-angle for angle
(def-split-key-constructor angle degrees
    ((radians radians-args (radians))
     (degrees degrees-args (degrees))
     (turns turns-args (turns))))

(defmethod radians ((angle degrees))
  "Get radians value from a degrees instance."
  (* (degrees angle)
     (/ pi 180)))

(defmethod radians ((angle turns))
  "Get radians value from a turns instance."
  (* (turns angle)
     (* pi 2)))

(defmethod degrees ((angle radians))
  "Get degrees value from a radians instance."
  (/ (radians angle)
     (/ pi 180)))

(defmethod degrees ((angle turns))
  "Get degrees value from a turns instance."
  (* (turns angle)
     360))

(defmethod turns ((angle radians))
  "Get turns value from a radians instance."
  (/ (radians angle)
     (* pi 2)))

(defmethod turns ((angle degrees))
  "Get turns value from a degrees instance."
  (/ (degrees angle)
     360))

;;; examples
;(turns (make-angle)) ;; => 0
;(degrees (make-radians pi)) ;; => 180
;(radians (make-radians pi)) ;; => pi
;(turns (make-radians pi)) ;; => 0.5
;(degrees (make-degrees 90)) ;; => 90
;(radians (make-degrees 90)) ;; => pi / 2
;(turns (make-degrees 90)) ;; => 1/4
;(degrees (make-angle :radians (* 2 pi))) ;; => 360
;(turns (make-angle :degrees 180)) ;; => 1/2
;(radians (make-angle :turns 1/2)) ;; => pi
;(make-angle :radians pi :degrees 90) ;; error! ambiguous
