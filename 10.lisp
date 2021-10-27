(ql:quickload :fare-memoization)

(defparameter *output* (cons 0
			     (sort (mapcar #'parse-integer
					   (uiop:read-file-lines "day10.txt"))
				   #'<)))

(let* ((data (append *output*
		     (list (+ 3 (apply #'max *output*)))))
       (result (loop for (x y) on data
		     while y
		     collect (- y x))))
  (print (* (count 1 result)
	    (count 3 result))))

(defun count-combinations (adaptors)
  (if (= 1 (length adaptors))
      1
      (loop with start = (car adaptors)
	    for (val . other-adaptors) on (cdr adaptors)
	    while (<= (- val start) 3)
	    sum (count-combinations (cons val other-adaptors)))))
