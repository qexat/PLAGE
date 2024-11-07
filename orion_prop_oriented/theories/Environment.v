Require List.
Require String.
Import List.ListNotations.

Require Context.
Require Hypotheses.
Require Proposition.
Require Term.

Record t := makeEnv { hypotheses : Hypotheses.t ; context : Context.t }.

Definition empty : t := {| hypotheses := nil ; context := nil |}.

Definition create (hypotheses : Hypotheses.t) : t :=
  {| hypotheses := hypotheses ; context := Context.of_hypotheses hypotheses |}.

Definition define_variable (environment : t) (name : String.string) : t :=
  let variable := Term.TVar name in
  let prop := Proposition.PDefined variable in
  {| hypotheses := (List.app environment.(hypotheses) [prop])
  ; context := (List.app environment.(context) [name])
  |}.

Open Scope list_scope.

Fixpoint define_variables (environment : t) (names : list String.string) : t :=
  match names with
  | [] => environment
  | head :: tail => define_variables (define_variable environment head) tail
  end.

Close Scope list_scope.
