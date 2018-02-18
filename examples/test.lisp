;; here i'll just play around i suppose :O

(defpackage :graphics-test
  (:use :cl :graphics.image :graphics.angle :graphics.color :graphics.util))
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

(aref #(1 2 3) 3)


(mapcar #'max '(1 2 3) '( 4 5 0))
