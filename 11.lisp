(defparameter *seat-map* (uiop:read-file-lines "day11.txt"))

(defun push-extend (item place)
  (setf place (push item
		    (cdr (nthcdr (1- (length place))
				 place)))))

(defun extend-map-with-floor (map)
  (let* ((floor-row (make-string (+ 2 (length (car map)))
				 :initial-element #\.))
	 (enlarged-map (loop for row in map
			     collect (format nil ".~a." row))))
    (push floor-row enlarged-map)
    (push-extend floor-row enlarged-map)
    enlarged-map))

(defun update-round (map)
  (loop with enlarged-map = (extend-map-with-floor map)
	with result-map = (extend-map-with-floor map)
	for r from 0
	for row in enlarged-map
	do (loop for seat across row
		 for c from 0
		 do (case seat
		      (#\L (if (occupy-p r c enlarged-map)
			       (setf (aref (elt result-map r) c) #\#)))
		      (#\# (if (leave-p r c enlarged-map)
			       (setf (aref (elt result-map r) c) #\L)))))
	finally (return result-map)))

(defun occupy-p (row col map)
  (let ((adjacent-seats
	  (loop for direction in '((1 0) (-1 0)
				   (0 1) (0 -1)
				   (1 1) (-1 -1)
				   (1 -1) (-1 1))
		collect (mapcar #'+ (list row col) direction))))
    (loop for seat in adjacent-seats
	  never (eql #\# (aref (elt map (first seat))
			       (second seat))))))

(defun leave-p (row col map)
  (let ((adjacent-seats
	  (loop for direction in '((1 0) (-1 0)
				   (0 1) (0 -1)
				   (1 1) (-1 -1)
				   (1 -1) (-1 1))
		collect (mapcar #'+ (list row col) direction))))
    (loop for seat in adjacent-seats
	  sum (if (eql #\# (aref (elt map (first seat))
				 (second seat)))
		  1
		  0) into count
	  finally (return (>= count 4)))))

(defun count-people (map)
  (loop for row in map
	sum (count #\# row)))

(defun solve-first-part ()
  (let ((ans (update-round *seat-map*))
	(counter 0))
    (loop do (progn
	       (setf ans (update-round ans))
	       (if (/= counter (count-people ans))
		   (setf counter (count-people ans))
		   (return counter)))
	  finally (return counter))))
