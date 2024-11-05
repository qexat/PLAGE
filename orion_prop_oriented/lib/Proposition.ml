(** Represents the proposition kinds. *)
type t =
  | Defined of Term.t
  | Implication of (t * t)
  | TypeOf of Term.t * Term.t

(** [show proposition] produces a printable view of the [proposition] *)
let rec show (proposition : t) : string =
  match proposition with
  | Defined variable -> "defined " ^ Term.show variable
  | Implication (left, right) -> show left ^ " -> " ^ show right
  | TypeOf (left, right) -> Term.show left ^ " : " ^ Term.show right
;;
