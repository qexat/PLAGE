(* To simplify this experiment, we don't do de Bruijn indexing here.
   Variables are simply their names. *)

(** Represents a variable. *)
type t = Var of string

(** [show variable] produces a printable view of the [variable]*)
let show : t -> string = function
  | Var name -> name
;;
