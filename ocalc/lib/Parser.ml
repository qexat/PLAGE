type t =
  { tokens : Token.t list
  ; mutable position : int
  }

let peek (parser : t) : Token.t = List.nth parser.tokens parser.position

let is_at_end (parser : t) : bool =
  let kind, _, _ = peek parser in
  match kind with
  | Token_kind.EOF -> true
  | _ -> false
;;

let advance (parser : t) = parser.position <- parser.position + 1

let consume (parser : t) : Token.t =
  let token = peek parser in
  advance parser;
  token
;;

let expect (parser : t) (expected : Token_kind.t) : Token.t option =
  let kind, _, _ = peek parser in
  match kind = expected with
  | false -> None
  | true -> Some (consume parser)
;;

let rec expect_one_of (parser : t) (expected : Token_kind.t list) : Token.t option =
  match expected with
  | [] -> None
  | kind :: rest ->
    let maybe_token = expect parser kind in
    if Option.is_some maybe_token then maybe_token else expect_one_of parser rest
;;

let matches (parser : t) (expected : Token_kind.t) : bool =
  Option.is_some (expect parser expected)
;;

let parse_atom (parser : t) : Ast.t option =
  Option.map (fun (_, lexeme, _) -> Ast.Literal lexeme) (expect parser Token_kind.INTEGER)
;;

let parse_unary (parser : t) : Ast.t option =
  Option.bind
    (expect_one_of parser [ Token_kind.PLUS; Token_kind.MINUS ])
    (fun (kind, _, _) ->
      Option.map (fun atom -> Ast.UnaryOp (kind, atom)) (parse_atom parser))
;;

let rec parse_binary
  (parser : t)
  (pred : t -> Ast.t option)
  (token_kinds : Token_kind.t list)
  : Ast.t option
  =
  match pred parser with
  | None -> None
  | Some left ->
    Option.bind (expect_one_of parser token_kinds) (fun (kind, _, _) ->
      Option.map
        (fun right -> Ast.BinaryOp (kind, left, right))
        (parse_binary parser pred token_kinds))
;;

let parse_sum (parser : t) : Ast.t option =
  parse_binary parser parse_unary [ Token_kind.PLUS; Token_kind.MINUS ]
;;

let parse_prod (parser : t) : Ast.t option =
  parse_binary parser parse_sum [ Token_kind.ASTERISK; Token_kind.SLASH ]
;;

let parse_expr (parser : t) : Ast.t option = parse_prod parser
let parse : t -> Ast.t option = parse_expr
