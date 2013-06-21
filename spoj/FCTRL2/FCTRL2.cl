(defun factorial (n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(loop repeat (read)
  do (print (factorial (read))))
