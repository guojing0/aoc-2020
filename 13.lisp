(ql:quickload :split-sequence)

(defparameter *note* (uiop:read-file-lines "day13.txt"))
(defparameter *depart-time* (parse-integer (first *note*)))
(defparameter *bus-ids* (second *note*))
(defparameter *valid-ids* (valid-bus-ids (all-bus-ids *bus-ids*)))

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
