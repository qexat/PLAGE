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

let term = Elaboration.expr_to_term expr
let () = Printf.printf "%s\n" (Core.typed_term_to_string term)
let equations = Typing.generate_equations term

let () =
  equations |> List.map Typing.type_equation_to_string |> List.iter (Printf.printf "%s\n")
;;
