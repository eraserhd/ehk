Require Import CpdtTactics Ascii String List Arith.

(* Needed for quoted character literals *)
Local Open Scope char_scope.

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

Definition ascii_of_letter (l : letter) : ascii :=
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
    end.
    crush.
  apply plus_le_compat_l with (p := 65) in gt_Z_proof.
  rewrite le_plus_minus_r in gt_Z_proof by assumption.
  simpl in gt_Z_proof.
  replace (nat_of_ascii c) with n. 
  replace (nat_of_ascii "A") with 65 by crush.
  replace (nat_of_ascii "A") with 65 in ge_A_proof by crush.
  replace (nat_of_ascii "Z") with 90 by crush.
  crush.
  crush.
  crush.
Defined.