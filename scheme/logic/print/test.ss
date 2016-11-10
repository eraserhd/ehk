#!/usr/bin/env scheme --program

(import (chezscheme)
        (logic print))

;; Basic statements are formatted
(assert (equal? (format-proposition '(A and B)) "(A and B)"))
