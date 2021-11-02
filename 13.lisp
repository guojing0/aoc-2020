(ql:quickload :split-sequence)

(defparameter *note* (uiop:read-file-lines "input/day13.txt"))
(defparameter *depart-time* (parse-integer (first *note*)))
(defparameter *bus-ids* (second *note*))
(defparameter *all-bus-ids* (all-bus-ids *bus-ids*))
(defparameter *valid-ids* (valid-bus-ids *all-bus-ids*))

(defun all-bus-ids (ids)
  (split-sequence:split-sequence #\, ids))

(defun valid-bus-ids (all-ids)
  (mapcar #'parse-integer
          (remove-if #'(lambda (s) (string= "x" s))
                     all-ids)))

(defun solve-first-part (bus-ids)
  (loop for time from *depart-time*
        do (loop for id in bus-ids
                 if (zerop (mod time id))
                   do (return-from solve-first-part (* (- time *depart-time*)
                                                       id)))))

(defun timestamp-pairs (valid-ids all-ids)
  (mapcar #'(lambda (n)
              (cons n (position (write-to-string n)
                                all-ids
                                :test #'string=)))
          valid-ids))

(defun crt-init-pairs (pairs)
  (mapcar #'(lambda (pair)
              (let ((id (car pair)))
                (cons id (mod (- id (cdr pair)) id))))
          pairs))

(defun solve-crt (pairs)
  (loop with ans = 0
        with inc = 1
        for (id . offset) in pairs
        do (loop while (/= (mod ans id) offset)
                 do (incf ans inc)
                 finally (setf inc (* inc id)))
        finally (return ans)))

(defun solve-second-part ()
  (solve-crt (crt-init-pairs (timestamp-pairs *valid-ids*
                                              *all-bus-ids*))))
