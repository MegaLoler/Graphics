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

(defmacro generate (size coordinate-syms dimension-syms generator)
  "Generate a pixmap given a pixel generator expression."
  (let ((pixmap-sym (gensym)))
    `(let ((,pixmap-sym (make-pixmap ,@size))
	   ,@(loop for dimension-sym in dimension-syms
		for dimension in size
		collect (list dimension-sym dimension)))
       (iterate ,pixmap-sym ,coordinate-syms ,(gensym)
		(setf (aref ,pixmap-sym ,@coordinate-syms) ,generator))
       ,pixmap-sym)))

(defun ppm-format-color (color &optional stream)
  "Format a single color for spaced ascii ppm."
  (format stream "~a ~a ~a"
	  (color-8-bit-red color)
	  (color-8-bit-green color)
	  (color-8-bit-blue color)))

(defun ppm-ascii (stream pixmap)
  "Format a pixmap as ppm data."
  (format stream "P3~%~a ~a~%255~%~a"
	  (array-dimension pixmap 0)
	  (array-dimension pixmap 1)
	  (join (iterate pixmap (x y) pixel (ppm-format-color pixel)))))

;; i'd really rather use symbols for format rather than strings? ie 'ppm
(defun get-image-format-function (format)
  "Get an image format function for the piven image format."
  (cond ((equal format "ppm") #'ppm-ascii)
	(t (error (format nil "Unknown image format ~a!" format)))))

(defun format-pixmap (stream pixmap format)
  "Format a pixmap as image data."
  (funcall (get-image-format-function format) stream pixmap))

(defun write-pixmap (pixmap filename &key format overwrite)
  "Write a pixmap to a file with format indicated by file extension, or override with :format."
  (let ((format (if format
		    (symbol-name format)
		    (get-filename-extension filename)))
	(if-exists (if overwrite :supersede)))
    (with-open-file (stream filename
			    :direction :output
			    :if-exists if-exists)
      (if stream
	  (format-pixmap stream pixmap format)
	  (error "Could not open file!")))))
