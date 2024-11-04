(** Represents the proposition kinds. *)
type t = Defined of Variable.t

(** [show proposition] produces a printable view of the [proposition] *)
let show (proposition : t) : string =
  match proposition with
  | Defined variable -> "defined " ^ Variable.show variable
;;
