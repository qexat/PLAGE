open Core2

module Type_constraint = struct
  type t = Type.t * Type.t

  let to_string : t -> string = function
    | left, right -> Printf.sprintf "%s = %s" (Type.to_string left) (Type.to_string right)
  ;;

  module Notation = struct
    let ( ==? ) (left : Type.t) (right : Type.t) : t = left, right
  end
end

let rec generate_constraints ({ ty; term } : Term.t) : Type_constraint.t list =
  let open Type.Notation in
  let open Type_constraint.Notation in
  match term with
  (* term := application argument
    application : (type of argument) -> (type of term') *)
  | App (application, argument) ->
    (application.ty ==? argument.ty --> ty)
    :: (generate_constraints application @ generate_constraints argument)
  (* term : parameter_type -> (type of body) *)
  | Fun (parameter, parameter_type, body) ->
    (ty ==? parameter_type --> body.ty) :: generate_constraints body
  (* term := let name = body
    term : ()
    name : type of body *)
  | Let (binding, binding_type, body) ->
    (ty ==? Type.unit) :: (binding_type ==? body.ty) :: generate_constraints body
  (* term := value
     term : Nat *)
  | Lit value -> [ ty ==? Type.nat ]
  (* term := name *)
  | Var name -> []
;;
