(in-package :graphics.raster)

;; this can probably be written a lot more cleanly?
;; also maybe offset the sampled coordinates by half a pixel's distance?
(defmacro rasterize (viewport-min-coords viewport-max-coords image-size color-function
		     &key (fg (make-color :saturation 0)) (bg (make-color)))
  "Render an array of colors by subdividing a region of space into cells and calculating each color by applying the coordinates of those cells to a color function."
  (let* ((dimensionality (length image-size))
	 (pixel-coord-syms (loop repeat dimensionality collect (gensym)))
	 (pixel-dimension-syms (loop repeat dimensionality collect (gensym)))
	 (viewport-range (loop for min in viewport-min-coords
			    for max in viewport-max-coords
			    collect (- max min))))
    `(generate ,image-size ,pixel-coord-syms ,pixel-dimension-syms
	       (funcall (csg-color-bg (csg-color-fg ,color-function ,fg) ,bg)
			(let* ((progress-coords (loop for coord in (list ,@pixel-coord-syms)
						  for size in (list ,@image-size)
						  collect (/ coord size)))
			      (mapped-coords (loop for progress in progress-coords
						for range in (list ,@viewport-range)
						for min in (list ,@viewport-min-coords)
						collect (+ min (* progress range)))))
			  (apply #'vector mapped-coords))))))
