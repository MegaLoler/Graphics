(in-package :graphics.util)

;;; generic util

;; i got this from stack overflow :p
(defun array-to-list (array)
  (let* ((dimensions (array-dimensions array))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (apply #'aref array indices)
                               (recurse (1+ n))))))
      (recurse 0))))

(defun concat (ls &optional (type 'string))
  "Concatenate a list of strings."
  (apply #'concatenate `(,type ,@ls)))

(defun join (ls &optional (delimiter " "))
  "Join a list of strings with a delimiter."
  (format nil (concatenate 'string "~{~a~^" delimiter "~}") ls))

(defun iterate-helper (array-sym all-indices indices item-sym body)
  "Helper function for the iterate macro below."
  (if indices
      `(loop for ,@(last indices)
	  from 0 to (1- (array-dimension ,array-sym ,(1- (length indices))))
	  append ,(iterate-helper array-sym all-indices (butlast indices) item-sym body))
      `(let ((,item-sym (aref ,array-sym ,@all-indices)))
	 (list (progn ,@body)))))

(defmacro iterate (array indices item-sym &body body)
  "Iterate over all the combinations in a multidimensional array."
  (let ((array-sym (gensym)))
    `(let ((,array-sym ,array))
       ,(iterate-helper array-sym indices indices item-sym body))))

(defun get-filename-extension (filename)
  "Get the extension substring of a filename string."
  (subseq filename (1+ (position #\. filename :from-end t))))

(defun make-keyword (name)
  "Make a keyword symbol from any symbol."
  (intern (symbol-name name) "KEYWORD"))

(defmacro concat-symbols (&body symbols)
  "Concatenate a series of symbols."
  `(intern (concatenate 'string ,@(loop for symbol in symbols collect (list 'symbol-name symbol)))))

(defun make-constructor-name-symbol (class-name)
  "Make a symbol suitable for naming a constructor given a class name."
  (concat-symbols 'make- class-name))

(defun multiple? (pred seq)
  "If pred returns true for more than 1 item in seq."
  (> (count-if pred seq) 1))

(defun truthy? (val)
  "Whether an object is not nil."
  (not (not val)))

(defun between? (val min max)
  "min <= val < max"
  (and (<= min val)
       (< val max)))

;;; constructor util

(defun make-constructor (class-name)
  "Make a simple anonymous constructor for a class."
  (lambda (&rest rest)
    (apply #'make-instance (append (list class-name) rest))))

(defmacro defconstructor (class-name slot-names)
  "Make a constructor for a class that only initializes slots specified."
  `(defmacro ,(make-constructor-name-symbol class-name)
       (&optional ,@slot-names)
     `(make-instance
       ',',class-name
       ,@(append
	  ,@(loop for slot-name in slot-names
			       collect `(if ,slot-name
					    `(,',(make-keyword slot-name)
						 ,,slot-name)))))))

(defmacro def-split-key-function (name default-func func-ls)
  "Define a function that calls different functions depending on which keywords are supplied."
  `(defun ,name (&key ,@(apply #'append
			       (loop for func-item in func-ls
				  for combined-arg = (cadr func-item)
				  for args = (caddr func-item)
				  collect (cons combined-arg args))))
     (if (multiple?
	  #'truthy?
	  (list
	   ,@(apply
	      #'append
	      (loop for func-item in func-ls
		 for combined-arg = (cadr func-item)
		 for args = (caddr func-item)
		 collect (list combined-arg
			       (cons 'or args))))))
	 (error "Ambiguous keywords!")
	 (let*
	     ,(apply
	       #'append
	       (loop for func-item in func-ls
		  for combined-arg = (cadr func-item)
		  for args = (caddr func-item)
		  collect
		    (append
		     (loop for arg in args
			for i from 0
			collect `(,arg (if ,combined-arg
					   (nth ,i ,combined-arg)
					   ,arg)))
		     (list (list combined-arg
				 (cons 'or args))))))
	   (cond
	     ,@(append 
		(loop for func-item in func-ls
		   for func = (car func-item)
		   for combined-arg = (cadr func-item)
		   for args = (caddr func-item)
		   collect
		     `(,combined-arg
		       (apply
			,func
			(apply
			 #'append
			 (loop
			    for slot-name in ',args
			    for slot-value in (list ,@args)
			    collect (if slot-value
					(list (make-keyword slot-name)
					      slot-value)))))))
		(list `( t (funcall
			    ,default-func)))))))))

(defmacro def-split-key-constructor (name default-class classes)
  "Make a function that constructs an instance of difference classes depending on the keyword arguments given."
  `(def-split-key-function ,(make-constructor-name-symbol name) (make-constructor ',default-class)
       ,(loop for class in classes
	   collect (cons `(make-constructor ',(car class))
			 (cdr class)))))
