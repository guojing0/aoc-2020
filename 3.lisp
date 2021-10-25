(defparameter *map* (uiop:read-file-lines "day3.txt"))
(defparameter *map-width* (length (car *map*)))
(defparameter *map-length* (length *map*))

;; A straight-forward way to complete first task
(print (count t
	      (loop for line in *map*  collect
				       (eql #\# (elt line
						     (rem (* 3 (position line *map*))
							  *map-width*))))))

;; A more mathematical solution for the rest...
