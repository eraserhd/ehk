#!/usr/bin/env scheme --program

(import (chezscheme)
        (logic print))

(assert (equal? (format-proposition 'A) "A"))
(assert (equal? (format-proposition '_) "⊥"))
(assert (equal? (format-proposition '(A => B)) "A ⇒ B"))
(assert (equal? (format-proposition '(A or B)) "A ∨ B"))
(assert (equal? (format-proposition '(A and B)) "A ∧ B"))
(assert (equal? (format-proposition '(A => _)) "¬A"))
