open Formatting

module Term = struct
  type t =
    | Application of t * t
    | Assignment of t * t
    | Constant_literal of string
    | Function_literal of t * t
    | Identifier of string
    | Infix_expr of t * string * t
    | Infix_stmt of t * string * t
    | Parameter of string * string
    | String_literal of string
    | Type_annotated of t * string
    | Type_identifier of string

  let is_atomic : t -> bool = function
    | Constant_literal _ | Identifier _ | String_literal _ -> true
    | _ -> false
  ;;

  let rec tokenize : t -> Token.t list = function
    | Application (app, arg) -> tokenize app @ [ Token.space ] @ tokenize arg
    | Assignment (name, body) ->
      [ Token.Type.Keyword, "let"; Token.space ]
      @ tokenize name
      @ [ Token.space; Token.Type.Operator_statement, "="; Token.space ]
      @ tokenize body
    | Constant_literal lexeme -> [ Token.Type.Constant_literal, lexeme ]
    | Function_literal (param, body) ->
      [ Token.Type.Keyword, "fun"; Token.space ]
      @ tokenize param
      @ [ Token.space; Token.Type.Operator_statement, "->"; Token.space ]
      @ tokenize body
    | Identifier lexeme -> [ Token.Type.Identifier, lexeme ]
    | Infix_expr (left, operator, right) ->
      tokenize left
      @ [ Token.space; Token.Type.Operator_expression, operator; Token.space ]
      @ tokenize right
    | Infix_stmt (left, operator, right) ->
      tokenize left
      @ [ Token.space; Token.Type.Operator_statement, operator; Token.space ]
      @ tokenize right
    | Parameter (lexeme, ty) ->
      [ Token.Type.Strong_punctuation, "(" ]
      @ [ Token.Type.Parameter, lexeme ]
      @ [ Token.space
        ; Token.Type.Strong_punctuation, ":"
        ; Token.space
        ; Token.Type.Type, ty
        ; Token.Type.Strong_punctuation, ")"
        ]
    | String_literal lexeme -> [ Token.Type.String, lexeme ]
    | Type_annotated (expr, ty) ->
      (if is_atomic expr then [] else [ Token.Type.Strong_punctuation, "(" ])
      @ tokenize expr
      @ (if is_atomic expr then [] else [ Token.Type.Strong_punctuation, ")" ])
      @ [ Token.space
        ; Token.Type.Strong_punctuation, ":"
        ; Token.space
        ; Token.Type.Type, ty
        ]
    | Type_identifier lexeme -> [ Token.Type.Type, lexeme ]
  ;;
end

module type ELABORABLE = sig
  type t

  val elaborate : t -> Term.t
end

module MakeFormatter (M : ELABORABLE) = struct
  let format ?(stylizer : stylizer = default_stylizer) (term : M.t) : string =
    format ~stylizer (M.elaborate term) (module Term)
  ;;
end
