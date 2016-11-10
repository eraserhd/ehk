(library (logic print)
  (export format-proposition
          parse-infix)
  (import (rnrs)
          (utils match))

  (define precedence
    '((¬ . 1)
      (and . 2)
      (or . 3) 
      (=> . 4)))

  (define (use-negation P)
    (match P
      [(,[A] => _) `(¬ A)]
      [(,[A] ,op ,[B]) `(,A ,op ,B)]
      [,A A]))

  (define (format-proposition P)
    (match (use-negation P)
      [(¬ ,[A]) (string-append "¬" A)]
      [(,[A] => ,[B]) (string-append A " ⇒ " B)]
      [(,[A] or ,[B]) (string-append A " ∨ " B)]
      [(,[A] and ,[B]) (string-append A " ∧ " B)]
      [_ "⊥"]
      [,A (guard (symbol? A)) (symbol->string A)]))

  (define (parse-infix expr)
    (car expr)))
