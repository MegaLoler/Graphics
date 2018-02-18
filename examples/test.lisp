;; here i'll just play around i suppose :O

(defpackage :graphics-test
  (:use :cl :graphics.image :graphics.angle :graphics.color :graphics.util :graphics.raster :graphics.geometry :graphics.transform :graphics.vector))
(in-package :graphics-test)

;; outfile for testing
(defvar *outfile* "~/misc/test.ppm")

;; save a 400x300 image of a certain solid color
(write-pixmap (make-pixmap 400 300 :fill (make-color :hue (make-turns 1/5)))
	      *outfile* :overwrite t)

;; graph a hue-value color space!
(write-pixmap (generate (600 400) (x y) (width height)
			(make-color :hue (make-turns (/ x width))
				    :value (/ y height)))
	      *outfile* :overwrite t)

;; graph a red-green color space!
(write-pixmap (generate (600 400) (x y) (width height)
			(make-color :red (/ x width)
				    :green (/ y height)))
	      *outfile* :overwrite t)

;; render a scene with csg!
(defparameter *csg-scene*
  (csg-union
   (csg-difference (csg-transform (make-csg-circle)
				  (make-translation (vector 1 1)))
		   (csg-transform (make-csg-circle)
				  (make-scaling (vector 3 2/3))))
   (csg-color-fg
    (csg-transform
     (csg-transform (make-csg-circle)
		    (make-scaling (vector 1/5 2/3)))
     (make-translation (vector -1)))
    (make-color :hue (make-turns 1/15)))))
(write-pixmap (rasterize (-3 -2) (3 2) (300 200)
			 *csg-scene*
			 :fg (make-color :hue (make-turns 1/5))
			 :bg (make-color :hue (make-turns 3/5)))
	      *outfile* :overwrite t)
