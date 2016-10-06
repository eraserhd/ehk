Require Import CpdtTactics Ascii String List Arith Omega.
Require Coq.Vectors.Fin.

(* Needed for quoted character literals *)
Local Open Scope char_scope.
Local Open Scope string_scope.

Definition letter := Fin.t 26.

Fixpoint nat_of_letter {x} (l : Fin.t x) : nat :=
  match l with
  | Fin.F1 => 0
  | Fin.FS l' => S (nat_of_letter l')
  end.

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

Lemma lengths_sum : forall (a b : string), String.length (a ++ b) = String.length a + String.length b.
  induction a; induction b; crush.
Qed.

Lemma spaces_n_has_length_n : forall n : nat, String.length (spaces n) = n.
  intro.
  induction n.
  crush.
  simpl.
  crush.
Qed.

Lemma string_c_empty_length_is_1 : forall c : ascii, String.length (String c "") = 1.
  crush.
Qed.

Definition diamond_row {n} (l : Fin.t n) (half_width : nat) (hw_ge_l : half_width >= nat_of_letter l) : string :=
  let l_string := String (ascii_of_letter l) "" in
  match l with
  | Fin.F1 => spaces half_width ++ l_string ++ spaces half_width
  | Fin.FS l' => let inner := nat_of_letter l' in
                let outer := half_width - inner - 1 in
                spaces outer ++ l_string ++ spaces inner ++ " "%string ++ spaces inner ++ l_string ++ spaces outer
  end.

Theorem pred_of_letter_successor :
  forall (n : nat) (l : Fin.t n), pred (nat_of_letter (Fin.FS l)) = nat_of_letter l.
  crush.
Qed.

Lemma funny_minus_assoc :  forall n m : nat, n >= m -> (m + (n - m)) = (m + n - m).
  crush.
Qed.

Lemma foo :
  forall (n : nat) (x : Fin.t n) (a : string),
  String.length match x with | Fin.F1 => a | Fin.FS _ => b end =
  match x with | Fin.F1 => String.length a | Fin.FS _ => String.length b end.
  intros; induction x; crush.
Qed.

Theorem xxx : 
  forall (l : letter) (half_width : nat) (hw_ge_l : half_width >= nat_of_letter l),
  String.length (diamond_row l half_width hw_ge_l) = 2 * half_width + 1.

  intros.
  unfold diamond_row.
  rewrite foo.

  repeat match goal with
                  | [ |- context[String.length (_ ++ _)] ] => rewrite lengths_sum
                  | [ |- context[String.length (spaces _)] ] => rewrite spaces_n_has_length_n
                  | [ |- context[String.length (String _ "")] ] => rewrite string_c_empty_length_is_1
                  | [ |- context[pred (nat_of_letter (Fin.FS _))] ] => rewrite pred_of_letter_successor
                  end.

  crush.
  rewrite funny_minus_assoc.
  rewrite minus_plus with (n := 1) (m := half_width - nat_of_letter t).
  rewrite funny_minus_assoc.
  rewrite minus_plus.
  repeat rewrite plus_assoc.

  generalize (nat_of_letter t).
  intro.
  replace (1 + 1) with 2 by crush.

  rewrite minus_plus.
  

  

  replace (pred (nat_of_letter (Fin.FS t))) with (nat_of_letter t).
  induction n.
  replace (nat_of_letter Fin.F1) with 0 by crush.

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