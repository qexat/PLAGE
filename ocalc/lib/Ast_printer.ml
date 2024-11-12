let rec render (ast : Ast.t) : string =
  match ast with
  | BinaryOp (operator, left, right) ->
    "(" ^ Token_kind.show operator ^ " " ^ render left ^ " " ^ render right ^ ")"
  | Literal lexeme -> lexeme
  | UnaryOp (operator, right) -> "(" ^ Token_kind.show operator ^ render right ^ ")"
;;

let print (ast : Ast.t) = Printf.printf "%s\n" (render ast)
