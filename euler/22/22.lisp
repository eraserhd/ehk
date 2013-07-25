
(defun read-name (input)
  (let ((name (read input nil)))
    (read-char input nil)
    name))

(let ((names ())
      (accumulator 0))

  (with-open-file (input #p"names.txt" :direction :input)
    (loop
      (push (or (read-name input) (return)) names)))

  (setf names (sort names #'string<))

  (loop for name in names
	for index from 1
	do (setf accumulator (+ accumulator
				(* index
				   (loop for c across name
					 sum (+ 1 (- (char-int c)
						     (char-int #\A))))))))

  (format t "~A~%" accumulator))
