(defparameter *instrs* (uiop:read-file-lines "day12.txt"))
(defparameter *location* (make-list 2 :initial-element 0))
(defparameter *direction* 0) 

(defun gen-instrs (instrs)
  (mapcar #'(lambda (instr)
              (cons (char instr 0)
                    (parse-integer instr :start 1)))
          instrs))

(defun get-direction (dir)
  (let ((std-dir (mod dir 360)))
    (case std-dir
      (0 #\E)
      (90 #\N)
      (180 #\W)
      (270 #\S))))

(defun travel (instr)
  (let ((action (car instr))
        (amount (cdr instr)))
    (case action
      (#\E (incf (first *location*) amount))
      (#\W (decf (first *location*) amount))
      (#\N (incf (second *location*) amount))
      (#\S (decf (second *location*) amount))

      (#\L (incf *direction* amount))
      (#\R (decf *direction* amount))

      (#\F (travel (cons (get-direction *direction*)
                         amount))))))
