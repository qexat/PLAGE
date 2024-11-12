type t =
  | INTEGER
  | PLUS
  | MINUS
  | ASTERISK
  | SLASH
  | EOF

let show : t -> string = function
  | INTEGER -> "INTEGER"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | ASTERISK -> "ASTERISK"
  | SLASH -> "SLASH (/)"
  | EOF -> "EOF"
;;
