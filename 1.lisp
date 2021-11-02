(defparameter *numbers*
  (mapcar #'parse-integer
	  (uiop:read-file-lines "input/day1.txt")))

(loop named top
      for x in *numbers* do
	(loop for y in *numbers*
	      when (= 2020 (+ x y))
		do (return-from top (print (* x y)))))

(loop named top
      for x in *numbers* do
	(loop for y in *numbers* do
	  (loop for z in *numbers*
		when (= 2020 (+ x y z))
		  do (return-from top (print (* x y z))))))
