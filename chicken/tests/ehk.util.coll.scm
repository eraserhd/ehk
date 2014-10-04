
(test-group "update"
  (test '(1 5 3) (update '(1 2 3) 1 (cut + <> 3))))

(test-group "update-in"
  (test '((1 2 6) 4) (update-in '((1 2 3) 4) '(0 2) (cut + <> 3))))
