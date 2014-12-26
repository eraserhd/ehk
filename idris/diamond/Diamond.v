Require Import CpdtTactics Ascii String List Arith.
Require Coq.Vectors.Fin.

(* Needed for quoted character literals *)
Local Open Scope char_scope.
Local Open Scope string_scope.

Definition letter := Fin.t 26.

Fixpoint nat_of_letter {x} (l : Fin.t x) : nat :=
  proj1_sig (Fin.to_nat l).

Definition ascii_of_letter {n} (l : Fin.t n) : ascii :=
  ascii_of_nat (nat_of_letter l + 65).

Definition is_ascii_letter (c : ascii) : Prop :=
  let n := nat_of_ascii c in
    n >= (nat_of_ascii "A") /\ n <= (nat_of_ascii "Z").

Definition letter_of_ascii (c : ascii) : letter + {~ is_ascii_letter c}.
  refine (let n := nat_of_ascii c in
          match ge_dec n (nat_of_ascii "A") with
          | left ge_A_proof => match Fin.of_nat (n - 65) 26 with
                               | inleft l => inleft l
                               | inright gt_Z_proof => inright _
                               end
          | right not_ge_A_proof => inright _ 
          end);
    unfold is_ascii_letter;
    repeat match goal with
    | [ |- ~ _ ] => unfold not; intros
    | [ H : context[nat_of_ascii c] |- _ ] => replace (nat_of_ascii c) with n in H by crush
    | [ H : context[nat_of_ascii "A"] |- _ ] => replace (nat_of_ascii "A") with 65 in H by crush
    | [ H : context[nat_of_ascii "Z"] |- _ ] => replace (nat_of_ascii "Z") with 90 in H by crush
    | [ gt_Z_proof : _ - 65 >= 26  |- _ ] => apply plus_le_compat_l with (p := 65) in gt_Z_proof;
                                             rewrite le_plus_minus_r in gt_Z_proof by assumption;
                                             simpl in gt_Z_proof
    end; crush.
Defined.

Fixpoint spaces (n : nat) : string :=
  match n with
  | O => ""
  | S n' => (" " ++ spaces n')%string
  end.

Definition diamond_row {n} (l : Fin.t n) (outer_padding inner_padding : nat) : string :=
  let l_string := String (ascii_of_letter l) "" in
  let inner := match l with
               | Fin.F1 _ => l_string
               | Fin.FS _ _ => l_string ++ spaces inner_padding ++ l_string
               end in
  spaces outer_padding ++ inner ++ spaces outer_padding.

Fixpoint half_diamond {n} (l : Fin.t n) (outer_padding inner_padding : nat) : list string :=
  diamond_row l outer_padding inner_padding ::
  match l with
    | Fin.F1 _ => nil
    | Fin.FS _ l' => half_diamond l' (S outer_padding) (pred (pred inner_padding))
  end.

Fixpoint diamond (l : letter) : list string :=
  let inner_padding := 2* nat_of_letter l in
  let half := half_diamond l O inner_padding in
  (rev half ++ tail half)%list.

Theorem all_lines_of_a_diamond_have_the_same_length :
  forall (l : letter) (a b : nat),
    a < length (diamond l) ->
    b < length (diamond l) ->
    String.length (nth a (diamond l) "") = String.length (nth b (diamond l) "").
  intros.