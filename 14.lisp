(ql:quickload :cl-ppcre)

(defparameter *init* (uiop:read-file-lines "input/day14.txt"))

(defun parse-instr (instr)
  (or (cl-ppcre:do-register-groups (mask)
          ("mask = ([X10]{36})" instr)
        (return-from parse-instr (cons 'mask mask)))

      (cl-ppcre:do-register-groups ((#'parse-integer addr val))
          ("mem\\[([0-9]+)\\] = ([0-9]+)" instr)
        (return-from parse-instr (cons addr val)))))

(defun load-all-instrs (input)
  (mapcar #'parse-instr input))

(defun integer->bit-vector (length n)
  (loop for i from (1- length) downto 0
        with bit-vec = (make-array length :element-type 'bit)
        with num = n
        do (progn
             (setf (aref bit-vec i) (rem num 2))
             (setf num (floor (/ num 2))))
        finally (return bit-vec)))

(defun bit-vector->integer (bit-vector)
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun masking (number mask)
  (bit-vector->integer (loop for n across (integer->bit-vector 36 number)
                             for m across mask
                             collect (if (parse-integer (string m) :junk-allowed t)
                                         (parse-integer (string m))
                                         n))))

(defun run-instrs (instrs)
  (loop with mem = (make-hash-table)
        with mask = nil
        for (addr . val) in instrs
        do (if (eql addr 'mask)
               (setf mask val)
               (setf (gethash addr mem) (masking val mask)))
        finally (return mem)))

(defun solve-first-part (instrs)
  (loop for k being the hash-value of (run-instrs (load-all-instrs instrs))
        sum k))
