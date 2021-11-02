(ql:quickload '("alexandria" "cl-ppcre"))

(defparameter *answers*
  (cl-ppcre:split "\\n\\n"
		  (alexandria:read-file-into-string "input/day6.txt")))

;; Part I

(print (loop for ans in *answers*
	     sum (length (remove-duplicates
			  (cl-ppcre:regex-replace-all "\\n" ans "")))))

;; Part II

(print (loop for ans in *answers*
	     sum (length (reduce #'intersection
				 (mapcar (lambda (a) (coerce a 'list))
					 (cl-ppcre:split "\\n" ans))))))
