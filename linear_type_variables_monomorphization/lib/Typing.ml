type type_equation = Core.ty * Core.ty

let ( ==? ) (left : Core.ty) (right : Core.ty) : type_equation = left, right
let ( --> ) (left : Core.ty) (right : Core.ty) : Core.ty = Core.Fun_type (left, right)

let rec generate_equations (term : Core.term) : type_equation list =
  let open Core in
  let { ty; term' } = term in
  match term' with
  (* term' := application argument
    application : (type of argument) -> (type of term') *)
  | App (application, argument) ->
    (application.ty ==? argument.ty --> ty)
    :: (generate_equations application @ generate_equations argument)
  (* term' : parameter_type -> (type of body) *)
  | Fun (parameter, parameter_type, body) ->
    (ty ==? parameter_type --> body.ty) :: generate_equations body
  (* term' := let name = body
     term' : ()
     name : type of body *)
  | Let (binding, binding_type, body) ->
    (ty ==? Prim_type Unit_type) :: (binding_type ==? body.ty) :: generate_equations body
  (* term' := value
     term' : Nat *)
  | Lit value -> [ ty ==? Prim_type Nat_type ]
  (* term' := name *)
  | Var name -> []
;;

let type_equation_to_string : type_equation -> string = function
  | left, right ->
    Printf.sprintf "%s = %s" (Core.ty_to_string left) (Core.ty_to_string right)
;;
