
Require Import Ascii.
Require Import Coq.Strings.String.

Fixpoint pipe_escape (name : string) : string :=
  match name with
  | EmptyString          => ""%string
  | String "|"%char rest => String "\"%char (String "|"%char (pipe_escape rest))
  | String "\"%char rest => String "\"%char (String "\"%char (pipe_escape rest))
  | String ch rest       => String ch (pipe_escape rest)
  end.

Theorem pipe_escape_injective : 
