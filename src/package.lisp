(defpackage :graphics.util
  (:use :cl)
  (:export make-keyword
	   concat-symbols
	   make-constructor-name-symbol
	   multiple?
	   truthy?
	   between?

	   make-constructor
	   defconstructor
	   def-split-key-function
	   def-split-key-constructor))

(defpackage :graphics.angle
  (:use :cl :graphics.util)
  (:export make-radians
	   make-degrees
	   make-turns
	   make-angle
	   radians
	   degrees
	   turns))

(defpackage :graphics.color
  (:use :cl :graphics.angle :graphics.util)
  (:export make-rgb-color
	   make-hsv-color
	   make-color
	   rgb-color
	   hsv-color
	   rgb-color-from-int
	   color-rgb
	   color-red
	   color-green
	   color-blue
	   color-hsv
	   color-hue
	   color-saturation
	   color-value
	   color-8-bit-red
	   color-8-bit-green
	   color-8-bit-bule
	   color-24-bit-rgb-little-endian
	   color-24-bit-rgb-big-endian
	   color-hex))
