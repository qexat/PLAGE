(** Context function (Γ) bound to an environment.
    To avoid lookup overhead, it is implemented as a list instead. *)
type t = Variable.t list

(** The empty context. *)
let empty : t = []

(** [of_hypotheses hypotheses] produces a context from the [hypotheses]*)
let of_hypotheses (hypotheses : Hypotheses.t) : t =
  let rec of_hypotheses_aux (hypotheses : Hypotheses.t) (acc : t) : t =
    match hypotheses with
    | [] -> List.rev acc
    | head :: tail ->
      of_hypotheses_aux
        tail
        (match head with
         | Proposition.Defined variable -> variable :: acc)
  in
  of_hypotheses_aux hypotheses []
;;

(** [show context] produces a printable view of the [context] *)
let show (context : t) : string =
  "\x1b[1mΓ\x1b[22m : "
  ^
  match context with
  | [] -> "()"
  | head :: tail ->
    List.fold_left
      (fun acc item -> acc ^ ", " ^ Variable.show item)
      (Variable.show head)
      tail
;;
