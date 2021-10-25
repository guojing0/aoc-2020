(ql:quickload '("split-sequence" "alexandria"))

(print (count-if
	(lambda (pswd-line)
	  (destructuring-bind (range ch-colon pswd)
	      (split-sequence:split-sequence #\Space pswd-line)
	    (destructuring-bind (min-range max-range)
		(split-sequence:split-sequence #\- range)
	      (let ((ch (coerce (subseq ch-colon 0 1) 'character))
		    (min-range (parse-integer min-range))
		    (max-range (parse-integer max-range)))
		(flet ((in-range (n min max)
			 (and (>= n min) (<= n max))))
		  (in-range (count ch pswd) min-range max-range))))))
	(uiop:read-file-lines "day2.txt")))


(print (count-if
	(lambda (pswd-line)
	  (destructuring-bind (range ch-colon pswd)
	      (split-sequence:split-sequence #\Space pswd-line)
	    (destructuring-bind (first-pos second-pos)
		(split-sequence:split-sequence #\- range)
	      (let ((ch (find-if #'alpha-char-p ch-colon)))
		(alexandria:xor (eql ch (elt pswd (1- (parse-integer first-pos))))
				(eql ch (elt pswd (1- (parse-integer second-pos)))))))))
	(uiop:read-file-lines "day2.txt")))
