;; Incomplete solution; the idea is right.

(defparameter *instrs* (uiop:read-file-lines "input/day12.txt"))

(defun gen-instrs (instrs)
  (mapcar #'(lambda (instr)
              (cons (char instr 0)
                    (parse-integer instr :start 1)))
          instrs))

(defun rotate (waypoint deg)
  (if (zerop deg)
      waypoint
      (rotate (cons (cdr waypoint) (- (car waypoint)))
              (- deg 90))))

(defun forward-by-waypoint (loc waypoint delta)
  (cons (+ (car loc) (* delta (car waypoint)))
        (+ (cdr loc) (* delta (cdr waypoint)))))

(defun travel (instr ship waypoint)
  (let ((action (car instr))
        (amount (cdr instr)))
    (case action
      (#\E (incf (car waypoint) amount))
      (#\W (decf (car waypoint) amount))
      (#\N (incf (cdr waypoint) amount))
      (#\S (decf (cdr waypoint) amount))

      (#\L (setf waypoint (rotate waypoint (- 360 amount))))
      (#\R (setf waypoint (rotate waypoint amount)))

      (#\F (setf ship (forward-by-waypoint ship
                                           waypoint
                                           amount))))))

(defun manhattan-dist (loc)
  (+ (abs (car loc)) (abs (cdr loc))))
