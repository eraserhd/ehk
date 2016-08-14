#!/usr/bin/env scheme --program

(import (chezscheme)
        (lambda run))

(assert (equal? 42 (run '(TRUE 42 79)))) ; TRUE returns first arg
(assert (equal? 79 (run '(FALSE 42 79)))) ; FALSE returns second arg

(assert (equal? 42 (run '(IF TRUE 42 79))))
(assert (equal? 79 (run '(IF FALSE 42 79))))

(assert (equal? (run 'TRUE) (run '(AND TRUE TRUE))))
(assert (equal? (run 'FALSE) (run '(AND TRUE FALSE))))
(assert (equal? (run 'FALSE) (run '(AND FALSE TRUE))))
(assert (equal? (run 'FALSE) (run '(AND FALSE FALSE))))

(assert (equal? (run 'TRUE) (run '(NOT FALSE))))
(assert (equal? (run 'FALSE) (run '(NOT TRUE))))

(assert (equal? (run 'TRUE) (run '(OR TRUE FALSE))))
(assert (equal? (run 'TRUE) (run '(OR FALSE TRUE))))
(assert (equal? (run 'TRUE) (run '(OR TRUE TRUE))))
(assert (equal? (run 'FALSE) (run '(OR FALSE FALSE))))
