(defparameter *boarding-passes* (uiop:read-file-lines "input/day5.txt"))

(defun modify (pass)
  (nsubstitute #\1 #\B pass)
  (nsubstitute #\0 #\F pass)
  (nsubstitute #\1 #\R pass)
  (nsubstitute #\0 #\L pass)
  pass)

(defun seat-id (mod-pass)
  (+ (* 8 (parse-integer mod-pass :end 7 :radix 2))
     (parse-integer mod-pass :start 7 :radix 2)))

(print (loop for pass in *boarding-passes*
	     maximize (seat-id (modify pass))))

;; Part II

(defparameter *mod-boarding-passes*
  (loop for pass in *boarding-passes*
	collect (seat-id (modify pass))))

(print (loop for (x y) on (sort *mod-boarding-passes* #'<)
	     while y
	     when (= 2 (- y x))
	       return (1+ x)))
