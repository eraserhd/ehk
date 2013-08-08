
(defvar *start-state* #b111111111111110)

(defun end-state-p (state)
  (= 1 (logcount state)))

(defvar *sequences* '((0 2 5 9 14) ; Moving down and right
                      (1 4 8 13)
		      (3 7 12)
		      (0 1 3 6 10) ; Moving down and left
		      (2 4 7 11)
		      (5 8 12)
		      (3 4 5)      ; Moving left-to-right
		      (6 7 8 9)
		      (10 11 12 13 14)))

(defun reverse-3-bits (state bits)
  (loop with result = state
        for bit in bits
	for bit-number from 0 to 2
        do (setf result (logxor (ash 1 bit) result))
	finally (return result)))

(defun can-jump (state bits)
  (and (>= (length bits) 3)
       (logtest state (ash 1 (second bits)))
       (not (eq (logtest state (ash 1 (first bits)))
		(logtest state (ash 1 (third bits)))))))

(defun next-states (state)
  (loop with result = ()
	for sequence in *sequences*
	do (loop for possible-jump on sequence
		 when (can-jump state possible-jump)
		 do (push (reverse-3-bits state possible-jump) result))
	finally (return result)))

(defun solve ()
  (let ((states (list *start-state*))
	(found (make-array 32768 :initial-element -1))
	(states-for-next-iteration ()))
    (loop
      (setf states-for-next-iteration ())

      (dolist (state states)
	(if (end-state-p state)
	  (let ((path ()))
	    (loop while (not (= state -1))
		  do (push state path)
		     (setf state (aref found state)))
	    (return-from solve path)))

	(dolist (next-state (next-states state))
	  (if (= -1 (aref found next-state))
	    (progn
	      (setf (aref found next-state) state)
	      (push next-state states-for-next-iteration)))))
      (setf states states-for-next-iteration)
      (if (null states)
	(return-from solve nil)))))

(defvar *template*
  (concatenate 'string
    "    ~A~%"
    "   ~A ~A~%"
    "  ~A ~A ~A~%"
    " ~A ~A ~A ~A~%"
    "~A ~A ~A ~A ~A~%"
    "~%"))

(defun print-state (state)
  (apply #'format t *template*
	 (loop for i from 0 to 14
	       collect (if (logtest (ash 1 i) state)
			 "X"
			 "."))))
