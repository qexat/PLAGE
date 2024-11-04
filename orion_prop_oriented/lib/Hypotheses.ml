(** List of propositions that are held true. *)
type t = Proposition.t list

(** [show hypotheses] produces a printable view of the [hypotheses] *)
let show (hypotheses : t) : string =
  "\x1b[1mε\x1b[22m : "
  ^
  match hypotheses with
  | [] -> "()"
  | head :: tail ->
    List.fold_left
      (fun acc item -> acc ^ ", " ^ Proposition.show item)
      (Proposition.show head)
      tail
;;
