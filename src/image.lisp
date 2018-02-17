(in-package :graphics.image)

;; how to make the funcions below methods that only accepts pixmaps ?
;; not sure what i'm doing here tbh
(deftype pixmap (width height)
  `(array graphics.color::color (,width ,height)))

(defun make-pixmap (width height &key (fill (make-color)))
  "Create a new image buffer."
  (make-array (list width height)
	      :element-type 'graphics.color::color
	      :initial-element fill))

(defun ppm-format-color (stream color &optional colon at)
  "Format a single color for spaced ascii ppm."
  (declare (ignore colon at))
  (format stream "~a ~a ~a"
	  (color-8-bit-red color)
	  (color-8-bit-green color)
	  (color-8-bit-blue color)))

(defun ppm-ascii (stream pixmap)
  "Format a pixmap as ppm data."
  (let ((width (array-dimension pixmap 0))
	(height (array-dimension pixmap 1)))
    (format stream "P3~%~a ~a~%255~%~{~{~/graphics.image::ppm-format-color/~^ ~}~^ ~}"
	    width height (array-to-list pixmap))))
