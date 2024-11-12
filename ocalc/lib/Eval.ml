let binary_op_to_function : Token_kind.t -> int -> int -> int = function
  | Token_kind.PLUS -> Int.add
  | Token_kind.MINUS -> Int.sub
  | Token_kind.ASTERISK -> Int.mul
  | Token_kind.SLASH -> fun a b -> if b = 0 then 0 else Int.div a b
  | _ -> failwith "unreachable"
;;

let unary_op_to_function : Token_kind.t -> int -> int = function
  | Token_kind.PLUS -> fun i -> i
  | Token_kind.MINUS -> Int.neg
  | _ -> failwith "unreachable"
;;

let rec eval : Ast.t -> int = function
  | Ast.BinaryOp (operator, left, right) ->
    (binary_op_to_function operator) (eval left) (eval right)
  | Ast.Literal literal -> int_of_string literal (* will never fail *)
  | Ast.UnaryOp (operator, right) -> (unary_op_to_function operator) (eval right)
;;
