type t =
  | BinaryOp of Token_kind.t * t * t
  | Literal of string
  | UnaryOp of Token_kind.t * t
