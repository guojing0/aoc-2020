(ql:quickload '("cl-ppcre" "alexandria"))

(defparameter *boot-code* (uiop:read-file-lines "day8.txt"))

(defun instr-to-symbol (str)
  (alexandria:eswitch (str :test #'string=)
    ("acc" 'acc)
    ("jmp" 'jmp)
    ("nop" 'nop)))

(defun load-instrs ()
  (loop for i in *boot-code*
	collect (let ((curr-instr (cl-ppcre:split " " i)))
		  (list (instr-to-symbol (first curr-instr))
			(parse-integer (second curr-instr))))))

(defun run-console (code)
  (loop with acc = 0
	with pc = 0
	with len = (length code)
	with visited-table = (make-hash-table)
	do (progn

	     ;; Success!
	     (when (= pc len)
	       (return (list acc :success)))

	     ;; if program counter exceeds length of `code`
	     ;; then return `acc`
	     (when (> pc len)
	       (return (list acc :impossible)))

	     ;; if `instr` was run, then return `acc`
	     (let ((instr (elt code pc)))
	       (when (gethash pc visited-table)
		 (return (list acc :inf-loop)))

	       (setf (gethash pc visited-table) 1)

	       (alexandria:eswitch ((first instr) :test #'eql)
		 ('acc (progn
			 (incf acc (second instr))
			 (incf pc)))
		 ('jmp (incf pc (second instr)))
		 ('nop (incf pc)))))))

(defun solve-first-part ()
  (run-console (load-instrs)))

(defun solve-second-part ()
  (loop with code = (load-instrs)
	for i from 0 below (length code)
	do (let ((curr-run (elt code i))
		 (result nil))
	     (alexandria:eswitch ((first curr-run) :test #'eql)
	       ('acc t)
	       ('jmp (setf (elt code i) (list 'nop (second curr-run))))
	       ('nop (setf (elt code i) (list 'jmp (second curr-run)))))
	     (setf result (run-console code))
	     (if (eql :success (second result))
		 (return (first result)))
	     (setf (elt code i) curr-run))))
