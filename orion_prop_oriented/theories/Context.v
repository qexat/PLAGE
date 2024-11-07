Require Import String.
Require List.

Require Proposition.
Require Hypotheses.

Definition t := list string.

Definition of_hypotheses (hypotheses : Hypotheses.t) : t :=
  let fix of_hypotheses_aux (hypotheses : Hypotheses.t) (acc : t) : t :=
    match hypotheses with
    | nil => List.rev acc
    | cons head tail =>
      of_hypotheses_aux
        tail
        (match head with
          | Proposition.PDefined (Term.TVar name) => cons name acc
          | _ => acc end)
    end
    in of_hypotheses_aux hypotheses nil.

Definition show (context : t) : string :=
  "\x1b[1mΓ\x1b[22m : "
  ++
  match context with
  | nil => "()"
  | _ => concat ", " context
  end.
