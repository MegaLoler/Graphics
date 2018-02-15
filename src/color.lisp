;; todo:
;;   error when given args are wrong type?
;;   make subclasses of hsv for hsl, hsy, and hsi
;;   make a color subclass using color names for representation
;;   versions for all the accessors that take int designator

(in-package :graphics.color)

;; generic color class
(defclass color () ())

;; color stored as rgb
(defclass rgb-color (color)
  ((red :initform 0
	:initarg :red
	:accessor color-red
	:type (real 0 1))
   (green :initform 0
	  :initarg :green
	  :accessor color-green
	  :type (real 0 1))
   (blue :initform 0
	 :initarg :blue
	 :accessor color-blue
	 :type (real 0 1))))

;; color stored as hsv
(defclass hsv-color (color)
  ((hue :initform (make-angle)
	:initarg :hue
	:accessor color-hue
	:type angle)
   (saturation :initform 1
	       :initarg :saturation
	       :accessor color-saturation
	       :type (real 0 1))
   (value :initform 1
	  :initarg :value
	  :accessor color-value
	  :type (real 0 1))))

;; make the make-rgb-color constructor
(defconstructor rgb-color (red green blue))
  
;; make the make-hsv-color constructor
(defconstructor hsv-color (hue saturation value))

;; general constructor make-color for color
(def-split-key-constructor color rgb-color
    ((rgb-color rgb (red green blue))
     (hsv-color hsv (hue saturation value))))

(defmethod rgb-color ((color hsv-color))
  "Get an rgb-color from an hsv-color."
  (let* ((h (mod (turns (color-hue color)) 1))
	 (s (color-saturation color))
	 (v (color-value color))
	 (c (* v s))
	 (x (* c (- 1 (abs (- (mod (* h 6) 2) 1)))))
	 (m (- v c))
	 (rgbp (cond ((between? h 0/6 1/6) (make-rgb-color c x 0))
		     ((between? h 1/6 2/6) (make-rgb-color x c 0))
		     ((between? h 2/6 3/6) (make-rgb-color 0 c x))
		     ((between? h 3/6 4/6) (make-rgb-color 0 x c))
		     ((between? h 4/6 5/6) (make-rgb-color x 0 c))
		     ((between? h 5/6 6/6) (make-rgb-color c 0 x)))))
    (make-rgb-color (+ m (color-red rgbp))
		    (+ m (color-green rgbp))
		    (+ m (color-blue rgbp)))))

(defmethod hsv-color ((color rgb-color))
  "Get an hsv-color from an rgb-color."
  (let* ((rgb (color-rgb color))
	 (r (color-red color))
	 (g (color-green color))
	 (b (color-blue color))
	 (cmax (apply #'max rgb))
	 (cmin (apply #'min rgb))
	 (delta (- cmax cmin))
	 (h (cond ((= 0 delta)
		   (make-degrees 0))
		  ((= cmax r)
		   (make-degrees (* 60 (mod (/ (- g b) delta) 6))))
		  ((= cmax g)
		   (make-degrees (* 60 (+ (/ (- b r) delta) 2))))
		  ((= cmax b)
		   (make-degrees (* 60 (+ (/ (- r g) delta) 4))))))
	 (s (if (= cmax 0) 0 (/ delta cmax)))
	 (v cmax))
    (make-hsv-color h s v)))

(defun rgb-color-from-int (color &key (endianness 'little) (bits-per-channel 8))
  "Get an rgb-color from a integer representation."
  (cond ((equal endianness 'little)
	 (make-rgb-color (/ (ldb (byte bits-per-channel 0) color) 255)
			 (/ (ldb (byte bits-per-channel 8) color) 255)
			 (/ (ldb (byte bits-per-channel 16) color) 255)))
	((equal endianness 'big)
	 (make-rgb-color (/ (ldb (byte bits-per-channel 16) color) 255)
			 (/ (ldb (byte bits-per-channel 8) color) 255)
			 (/ (ldb (byte bits-per-channel 0) color) 255)))
	(t (error "Endianness must be little or big!"))))

(defmethod rgb-color ((color integer))
  "Get an rgb-color from a 24-bit integer representation."
  (rgb-color-from-int color))

(defmethod hsv-color ((color integer))
  "Get an hsv-color from an rgb 24-bit integer representation."
  (hsv-color (rgb-color color)))

(defmethod color-red ((color hsv-color))
  "Get the red component of an hsv-color instance."
  (color-red (rgb-color color)))

(defmethod color-green ((color hsv-color))
  "Get the green component of an hsv-color instance."
  (color-green (rgb-color color)))

(defmethod color-blue ((color hsv-color))
  "Get the blue component of an hsv-color instance."
  (color-blue (rgb-color color)))

(defmethod color-hue ((color rgb-color))
  "Get the hue component of an rgb-color instance."
  (color-hue (hsv-color color)))

(defmethod color-saturation ((color rgb-color))
  "Get the saturation component of an rgb-color instance."
  (color-saturation (hsv-color color)))

(defmethod color-value ((color rgb-color))
  "Get the value component of an rgb-color instance."
  (color-value (hsv-color color)))

(defmethod color-rgb ((color color))
  "Get the rgb components as a list."
  (list (color-red color)
	(color-green color)
	(color-blue color)))

(defmethod color-hsv ((color color))
  "Get the hsv components as a list."
  (list (color-hue color)
	(color-saturation color)
	(color-value color)))

(defmethod color-8-bit-red ((color color))
  "Get the red component of a color as an 8-bit value."
  (floor (* 255 (color-red color))))

(defmethod color-8-bit-green ((color color))
  "Get the green component of a color as an 8-bit value."
  (floor (* 255 (color-green color))))

(defmethod color-8-bit-blue ((color color))
  "Get the blue component of a color as an 8-bit value."
  (floor (* 255 (color-blue color))))

(defmethod color-24-bit-rgb-little-endian ((color color))
  "Get the 24-bit rgb representation of a color with red component as least significant byte."
  (+ (ash (color-8-bit-red color) 0)
     (ash (color-8-bit-green color) 8)
     (ash (color-8-bit-blue color) 16)))

(defmethod color-24-bit-rgb-big-endian ((color color))
  "Get the 24-bit rgb representation of a color with red component as most significant byte."
  (+ (ash (color-8-bit-red color) 16)
     (ash (color-8-bit-green color) 8)
     (ash (color-8-bit-blue color) 0)))

(defmethod color-hex ((color color))
  "Get the 6-digit hex code string representing a color."
  (format nil "#~6,'0x" (color-24-bit-rgb-big-endian color)))

;;; examples

;(make-rgb-color) ;; black
;(make-rgb-color 1) ;; red
;(make-rgb-color 0 1 0) ;; green
;(make-rgb-color 0 1 1) ;; cyan

;(make-hsv-color) ;; red
;(make-hsv-color nil nil 0) ;; black
;(make-hsv-color (make-angle) 0 0) ;; black
;(make-hsv-color (make-degrees 120)) ;; green
;(make-hsv-color nil 0) ;; white
;(make-hsv-color (make-angle) 0 0.5) ;; grey

;(make-color) ;; black
;(make-color :rgb '(1 1)) ;; yellow
;(make-color :rgb '(0 1 0)) ;; green
;(make-color :green 1) ;; green
;(make-color :blue 1 :green 1) ;; cyan
;(make-color :hsv '((make-degrees 240) 1 1)) ;; blue
;(make-color :hue (make-turns 1/3)) ;; green
;(make-color :saturation 0) ;; white
;(make-color :value 0.5) ;; dark red
;(make-color :rgb '(1 1 1) :hsv '((make-angle) 1 1)) ;; error! ambiguous
;(make-color :green 1 :hue (make-angle)) ;; error! ambiguous

;(turns (color-hue (make-rgb-color 1 0 1))) ;; (magenta) => 5/6
;(color-value (make-rgb-color 0 0.25 0.5)) ;; (dark sky blue) => 0.5
;(color-red (make-color :hue (make-turns 1/3))) ;; (green) => 0
;(color-blue (make-color :hue (make-turns 1/2))) ;; (cyan) => 1
;(color-hex (make-color :hue (make-degrees 30) :value 0.5)) ;; (dark orange) -> "#7F3F00
;(degrees (color-hue (rgb-color #x007FFF))) ;; (sky blue) => ~30
