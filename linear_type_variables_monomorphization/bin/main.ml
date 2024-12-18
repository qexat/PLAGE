open Linear_type_variables_monomorphization
open Grammar

let expr =
  Expr.Let
    { name = IDENTIFIER, "id", 0
    ; body =
        Expr.Function_literal
          { parameter = IDENTIFIER, "x", 0; body = Identifier (IDENTIFIER, "x", 0) }
    }
;;

let term = Elaboration.elaborate expr
let () = Printf.printf "%s\n" (Core.TermFormatter.format term)
let constraints = Typing.generate_constraints term

let () =
  constraints
  |> List.map Typing.Type_constraint.to_string
  |> List.iter (Printf.printf "%s\n")
;;
