(library (logic print)
  (export format-proposition)
  (import (rnrs)
          (utils match))

  (define (format-proposition P)
    (match P
      [(,[A] => _) (string-append "¬" A)]
      [(,[A] => ,[B]) (string-append A " ⇒ " B)]
      [(,[A] or ,[B]) (string-append A " ∨ " B)]
      [(,[A] and ,[B]) (string-append A " ∧ " B)]
      [_ "⊥"]
      [,A (guard (symbol? A)) (symbol->string A)])))
      
