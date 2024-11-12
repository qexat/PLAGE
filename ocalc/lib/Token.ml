type t = Token_kind.t * string * int

let show ((kind, lexeme, position) : t) : string =
  Printf.sprintf "%s \"%s\" %d" (Token_kind.show kind) (String.escaped lexeme) position
;;
