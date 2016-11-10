(library (logic print)
  (export format-proposition)
  (import (rnrs)
          (utils match))

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
      [,A (guard (symbol? A)) (symbol->string A)])))
      
