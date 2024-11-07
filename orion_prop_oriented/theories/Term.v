Require Import String.

Inductive t :=
  | TVar : string -> t
  | TApp : (t * t) -> t.

Fixpoint show (term : t) : string :=
  match term with
  | TVar name => name
  | TApp (func, arg) => (show func) ++ " -> " ++ (show arg)
  end.
