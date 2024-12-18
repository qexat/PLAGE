module Token_type = struct
  type t =
    (* key words/symbols *)
    | LET
    | FUN
    | EQUAL
    | RIGHT_ARROW
    (* terms *)
    | IDENTIFIER
    | NAT
    (* pairs *)
    | PAREN_LEFT
    | PAREN_RIGHT
    (* operators *)
    | PLUS
    | MINUS
    | STAR
    (* misc *)
    | EOF

  let to_string : t -> string = function
    | LET -> "let"
    | FUN -> "fun"
    | EQUAL -> "equal"
    | RIGHT_ARROW -> "right arrow"
    | IDENTIFIER -> "identifier"
    | NAT -> "nat"
    | PAREN_LEFT -> "paren left"
    | PAREN_RIGHT -> "paren right"
    | PLUS -> "plus"
    | MINUS -> "minus"
    | STAR -> "star"
    | EOF -> "eof"
  ;;
end

module Lexeme = struct
  type t = string
end

module Position = struct
  type t = int

  let to_string : t -> string = Int.to_string
end

module Token = struct
  type t = Token_type.t * Lexeme.t * Position.t

  let to_string : t -> string = function
    | token_type, lexeme, position ->
      Printf.sprintf
        "(%s, %s, %s)"
        (Token_type.to_string token_type)
        lexeme
        (Position.to_string position)
  ;;
end

module Expr = struct
  type t =
    | Identifier of Token.t
    | Nat_literal of Token.t
    (* --------------------- *)
    | Grouping of t
    (* --------------------- *)
    | Application of
        { application : t
        ; argument : t
        }
    (* --------------------- *)
    | Function_literal of
        { parameter : Token.t
        ; body : t
        }
    (* --------------------- *)
    | Let of
        { name : Token.t
        ; body : t
        }
end
