type t =
  | TVar of string
  | TApp of (t * t)

let rec show : t -> string = function
  | TVar name -> name
  | TApp (func, arg) -> show func ^ " -> " ^ show arg
;;
