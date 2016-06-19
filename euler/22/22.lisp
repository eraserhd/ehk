
(defun read-name (input)
  (let ((name (read input nil)))
    (read-char input nil)
    name))

(defun read-names ()
  (let ((names ()))
    (with-open-file (input #p"names.txt" :direction :input)
      (loop
	(push (or (read-name input) (return)) names)))
    names))

(defun name-sum (name)
  (loop for c across name
	sum (+ 1 (- (char-int c) (char-int #\A)))))

(defun compute-answer ()
  (let ((sorted-names (sort (read-names) #'string<))
	(accumulator 0))
    (loop for name in sorted-names
	  for index from 1
	  do (setf accumulator (+ accumulator
				  (* index (name-sum name)))))
    accumulator))

(format t "~A~%" (compute-answer))
