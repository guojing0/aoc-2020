(defparameter *reversed-seq* (reverse '(18 8 0 5 4 1 20)))

(defun next-number (seq)
  (let* ((last-num (car seq))
         (rest-seq (subseq seq 1))
         (num-pos (position last-num rest-seq)))
    (if (numberp num-pos)
        (1+ num-pos)
        0)))

(defun gen-seq (n seq)
  (if (zerop n)
      seq
      (gen-seq (1- n)
               (cons (next-number seq) seq))))

(defun solve-first-part ()
  (elt (gen-seq 2020 *reversed-seq*)
       (length *reversed-seq*)))

(defun solve-second-part ()
  (elt (gen-seq 30000000 *reversed-seq*)
       (length *reversed-seq*)))
