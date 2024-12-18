type token_type =
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

type lexeme = string
type position = int
type token = token_type * lexeme * position

module Expr = struct
  type t =
    | Identifier of token
    | Nat_literal of token
    (* --------------------- *)
    | Grouping of t
    (* --------------------- *)
    | Application of
        { application : t
        ; argument : t
        }
    (* --------------------- *)
    | Function_literal of
        { parameter : token
        ; body : t
        }
    (* --------------------- *)
    | Let of
        { name : token
        ; body : t
        }
end
