(** List of propositions that are held true. *)
type t = Proposition.t list

(** [show hypotheses] produces a printable view of the [hypotheses] *)
let show (hypotheses : t) : string =
  "\x1b[1mε\x1b[22m : "
  ^
  match hypotheses with
  | [] -> "()"
  | _ -> String.concat ", " (List.map Proposition.show hypotheses)
;;
