(proclaim '(optimize (speed 3) (debug 0)))

(defvar *dp* (make-array '(2005 2005) :element-type 'fixnum :adjustable nil))

(defun edit-distance (a b)
  (let* ((rows (+ 1 (length a)))
	 (cols (+ 1 (length b))))
    (dotimes (i rows)
      (setf (aref *dp* i 0) i))
    (dotimes (j cols)
      (setf (aref *dp* 0 j) j))
    (loop for i from 1 below rows
	  do (loop for j from 1 below cols
		   do (let* ((insert-cost (+ 1 (aref *dp* (- i 1) j)))
			     (delete-cost (+ 1 (aref *dp* i (- j 1))))
			     (chars-equal-p (eql (aref a (- i 1)) (aref b (- j 1))))
			     (change-cost (+ (aref *dp* (- i 1) (- j 1))
					     (if chars-equal-p 0 1)))
			     (cost (min insert-cost delete-cost change-cost)))
			(setf (aref *dp* i j) cost))))
    (aref *dp* (length a) (length b))))

(loop repeat (parse-integer (read-line))
      do (let ((a (read-line))
	       (b (read-line)))
	   (format t "~D~%" (edit-distance a b))))
