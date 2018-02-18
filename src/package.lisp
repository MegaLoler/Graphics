(defpackage :graphics.util
  (:use :cl)
  (:export array-to-list
	   resize-list
	   range
	   positions
	   get-filename-extension
	   concat
	   join
	   iterate
	   make-keyword
	   concat-symbols
	   make-constructor-name-symbol
	   multiple?
	   truthy?
	   between?
	   and-f
	   or-f
	   square

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
	   color-8-bit-blue
	   color-24-bit-rgb-little-endian
	   color-24-bit-rgb-big-endian
	   color-hex))

(defpackage :graphics.image
  (:use :cl :graphics.color :graphics.angle :graphics.util)
  (:export make-pixmap
	   generate
	   ppm-ascii
	   format-pixmap
	   write-pixmap))

(defpackage :graphics.vector
  (:use :cl :graphics.util)
  (:export vec-coords-in-bounds?
	   vec-max-dimensions
	   vec-get
	   vec-operation
	   vec-add
	   vec-subtract
	   vec-multiply
	   vec-divide
	   vec-multiply-scalar
	   vec-reverse
	   vec-distance))

(defpackage :graphics.transform
  (:use :cl :graphics.vector)
  (:export transform-vectors
	   translate
	   scale
	   make-transformation
	   make-translation
	   make-scaling
	   transform
	   reverse-transform))

(defpackage :graphics.geometry
  (:use :cl :graphics.util :graphics.vector :graphics.transform)
  (:export csg-n-sphere
	   make-csg-n-sphere
	   make-csg-circle
	   make-csg-sphere
	   csg-transform
	   csg-operation
	   csg-union
	   csg-intersection
	   csg-negation
	   csg-difference
	   csg-color-fg
	   csg-color-bg))

(defpackage :graphics.raster
  (:use :cl :graphics.image :graphics.color :graphics.geometry)
  (:export rasterize))
