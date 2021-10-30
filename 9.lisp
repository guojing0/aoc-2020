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

(defun find-sum-list (ans numbers)
  (let ((len (length numbers)))
    (loop for i from 0 below len
          do (loop for j from (1+ i) below len
                   for sum-list = (subseq numbers i j)
                   when (> (apply #'+ sum-list) ans)
                     return nil
                   when (= (apply #'+ sum-list) ans)
                     do (return-from find-sum-list sum-list)))))

(defun solve-second-part ()
  (let ((sum-list (find-sum-list (solve-first-part *numbers*) *numbers*)))
    (+ (apply #'max sum-list) (apply #'min sum-list))))
