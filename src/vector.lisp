(in-package :graphics.vector)

(defun vec-coords-in-bounds? (vector &rest coords)
  "Whether given coordinates exist inside vector."
  (and (= (length coords) (length (array-dimensions vector)))
       (reduce (lambda (a b) (and a b))
	       (mapcar (lambda (bound coord)
			 (and (>= coord 0)
			      (< coord bound)))
		       (array-dimensions vector) coords)
	       :initial-value t)))

(defun vec-max-dimensions (&rest vectors)
  "Get the outer dimensions of a group of vectors."
  (let* ((dimensions (mapcar #'array-dimensions vectors))
	 (dimensionalities (mapcar #'length dimensions))
	 (max-dimensionality (apply #'max dimensionalities))
	 (dimensions
	  (mapcar (lambda (dimensions)
		    (resize-list dimensions max-dimensionality 0))
		  dimensions))
	 (max-dimensions (apply #'mapcar (cons #'max dimensions))))
    max-dimensions))

(defun vec-get (vector &rest coords)
  "Get an item from an array, or nil if doesn't exist."
  (if (apply #'vec-coords-in-bounds? (cons vector coords))
      (apply #'aref (cons vector coords))))

(defun vec-operation (operation &rest vectors)
  "Perform an operation in parallel to some vectors."
  (let* ((result (make-array (apply #'vec-max-dimensions vectors)))
	 (positions (positions result)))
    (loop for coordinates in positions
       do (setf (apply #'aref (cons result coordinates))
		(apply
		 operation
		 (mapcar (lambda (vector)
		 	   (or (apply #'vec-get (cons vector coordinates))
			       0))
		 	 vectors))))
    result))

(defun vec-add (&rest vectors)
  "Add vectors."
  (apply #'vec-operation (cons #'+ vectors)))

(defun vec-subtract (&rest vectors)
  "Subtract vectors."
  (apply #'vec-operation (cons #'- vectors)))

(defun vec-multiply (&rest vectors)
  "Multiply vectors."
  (apply #'vec-operation (cons #'* vectors)))

(defun vec-divide (&rest vectors)
  "Divide vectors."
  (apply #'vec-operation (cons #'/ vectors)))

(defun vec-multiply-scalar (scalar vector)
  "Multiply vectors by a scalar value."
  (apply #'vector (mapcar (lambda (component)
			    (* scalar component))
			  (array-to-list vector))))

(defun vec-reverse (vector)
  "Reverse the direction a vector."
  (vec-multiply-scalar -1 vector))

(defun vec-distance (vector)
  "Get the distance of a vector ."
  (sqrt (apply #'+ (mapcar #'square (array-to-list vector)))))
