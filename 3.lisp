(defparameter *map* (uiop:read-file-lines "day3.txt"))
(defparameter *map-width* (length (car *map*)))
(defparameter *map-length* (length *map*))

;; A straight-forward way to complete first task
(print (count t
	      (loop for line in *map* collect
				      (eql #\# (elt line
						    (rem (* 3 (position line *map*))
							 *map-width*))))))

;; A more mathematical solution for the rest...
(defun travel (curr-x curr-y x-dir y-dir)
  (if (< curr-y *map-length*)
      (+ (travel (+ curr-x x-dir) (+ curr-y y-dir) x-dir y-dir)
	 (if (eql #\# (elt (nth curr-y *map*) (rem curr-x *map-width*)))
	     1
	     0))
      0))

(print (reduce #'* (loop for slope in '((1 . 1) (3 . 1) (5 . 1) (7 . 1) (1 . 2))
			 collect (travel 0 0 (car slope) (cdr slope)))))
