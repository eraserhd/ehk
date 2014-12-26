Require Import CpdtTactics Ascii String List Arith.

(* Needed for quoted character literals *)
Local Open Scope char_scope.
Local Open Scope string_scope.

(* This is basically stolen from Fin. *)
Inductive letterT : nat -> Set :=
| LO : forall {x}, letterT (S x)
| LS : forall {x}, letterT x -> letterT (S x). 

Definition letter := letterT 26.

Fixpoint nat_of_letter {x} (l : letterT x) : nat :=
  match l with
  | LO _ => O
  | LS _ l' => S (nat_of_letter l')
  end.

Definition ascii_of_letter {n} (l : letterT n) : ascii :=
  ascii_of_nat (nat_of_letter l + 65).

Lemma all_nat_ge_0 : forall n : nat, n >= 0.
  crush.
Qed.

Fixpoint letter_of_nat (n x : nat) : letterT x + {n >= x}. 
Proof.
  refine (match x with
          | O => inright (all_nat_ge_0 n)
          | S x' => match n with
                    | O => inleft LO
                    | S n' => match letter_of_nat n' x' with
                              | inleft l => inleft (LS l)
                              | inright p => _
                              end
                    end
          end).
  crush.  
Defined.

Definition is_ascii_letter (c : ascii) : Prop :=
  let n := nat_of_ascii c in
    n >= (nat_of_ascii "A") /\ n <= (nat_of_ascii "Z").

Definition letter_of_ascii (c : ascii) : letter + {~ is_ascii_letter c}.
  refine (let n := nat_of_ascii c in
          match ge_dec n (nat_of_ascii "A") with
          | left ge_A_proof => match letter_of_nat (n - 65) 26 with
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

Definition diamond_row {n} (l : letterT n) (outer_padding inner_padding : nat) : string :=
  let l_string := String (ascii_of_letter l) "" in
  let inner := match l with
               | LO _ => l_string
               | LS _ _ => l_string ++ spaces inner_padding ++ l_string
               end in
  spaces outer_padding ++ inner ++ spaces outer_padding.

Fixpoint half_diamond {n} (l : letterT n) (outer_padding inner_padding : nat) : list string :=
  diamond_row l outer_padding inner_padding ::
  match l with
    | LO _ => nil
    | LS _ l' => half_diamond l' (S outer_padding) (pred (pred inner_padding))
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