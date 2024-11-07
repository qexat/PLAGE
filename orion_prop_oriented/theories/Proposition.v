Require Import String.

Require Term.

Inductive t :=
  | PDefined : Term.t -> t
  | PImplication : (t * t) -> t
  | PTypeOf : (Term.t * Term.t) -> t.

Fixpoint show (proposition : t) : string :=
  match proposition with
  | PDefined variable => "defined " ++ (Term.show variable)
  | PImplication (pleft, pright) => (show pleft) ++ " -> " ++ (show pright)
  | PTypeOf (tleft, tright) => (Term.show tleft) ++ " : " ++ (Term.show tright)
  end.
