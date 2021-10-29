(defparameter *numbers* (mapcar #'parse-integer (uiop:read-file-lines "day9.txt")))

(defun in-sums-p (n sums)
  (loop for row in sums
	  thereis (numberp (position n row))))

(defun calculate-sums (list)
  (loop for (fst . rest-nums) on list
	while rest-nums
	collect (loop for snd in rest-nums
		      if (/= fst snd)
			collect (+ fst snd))))

(defun solve-first-part (numbers)
  (loop for target-index from 25 below (length numbers)
	for preamble-first-index from 0
	do (let ((premable (subseq numbers
				   preamble-first-index
				   (+ 25 preamble-first-index)))
		 (target-number (elt numbers target-index)))
	     (if (not (in-sums-p target-number
				 (calculate-sums premable)))
		 (return target-number)))))
