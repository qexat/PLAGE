Require Import String.

Require Proposition.

Definition t : Type := list Proposition.t.

Definition show (hypotheses : t) : string :=
  "\x1b[1mε\x1b[22m : "
  ++
  match hypotheses with
  | nil => "()"
  | _ => concat ", " (List.map Proposition.show hypotheses)
  end.
