open Formatting

module Fil = struct
  (** [Fil] encodes the Formatter Intermediate Language.

    A language λ that wish to get formatted can be elaborated to
    the FIL, which handles the tokenization and formatting of λ.
  *)

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

  (** [is_atomic term] determines whether [term] is atomic. *)
  let is_atomic : t -> bool = function
    | Constant_literal _ | Identifier _ | String_literal _ -> true
    | _ -> false
  ;;

  (** [tokenize term] transforms [term] into a sequence of
    formatting tokens. *)
  let rec tokenize : t -> Token.t list = function
    | Application (application, argument) ->
      tokenize application @ [ Token.space ] @ tokenize argument
    | Assignment (name, body) ->
      [ Token_type.Keyword, "let"; Token.space ]
      @ tokenize name
      @ [ Token.space; Token_type.Operator_statement, "="; Token.space ]
      @ tokenize body
    | Constant_literal lexeme -> [ Token_type.Constant_literal, lexeme ]
    | Function_literal (parameter, body) ->
      [ Token_type.Keyword, "fun"; Token.space ]
      @ tokenize parameter
      @ [ Token.space; Token_type.Operator_statement, "->"; Token.space ]
      @ tokenize body
    | Identifier lexeme -> [ Token_type.Identifier, lexeme ]
    | Infix_expr (left, operator, right) ->
      tokenize left
      @ [ Token.space; Token_type.Operator_expression, operator; Token.space ]
      @ tokenize right
    | Infix_stmt (left, operator, right) ->
      tokenize left
      @ [ Token.space; Token_type.Operator_statement, operator; Token.space ]
      @ tokenize right
    | Parameter (lexeme, ty) ->
      [ Token_type.Strong_punctuation, "(" ]
      @ [ Token_type.Parameter, lexeme ]
      @ [ Token.space
        ; Token_type.Strong_punctuation, ":"
        ; Token.space
        ; Token_type.Type, ty
        ; Token_type.Strong_punctuation, ")"
        ]
    | String_literal lexeme -> [ Token_type.String, lexeme ]
    | Type_annotated (term, ty) ->
      (if is_atomic term then [] else [ Token_type.Strong_punctuation, "(" ])
      @ tokenize term
      @ (if is_atomic term then [] else [ Token_type.Strong_punctuation, ")" ])
      @ [ Token.space
        ; Token_type.Strong_punctuation, ":"
        ; Token.space
        ; Token_type.Type, ty
        ]
    | Type_identifier lexeme -> [ Token_type.Type, lexeme ]
  ;;
end

module type ELABORABLE = sig
  (** [ELABORABLE] provides an interface for languages that wish
    to be formatted by getting elaborated to the FIL. *)

  (** [t] should encode the language λ. *)
  type t

  (** [elaborate term] transforms a [term] of a language λ into
      a term of the FIL. *)
  val elaborate : t -> Fil.t
end

module MakeFormatter (M : ELABORABLE) = struct
  (** Construct a formatter for the language provided by [M]. *)

  let format ?(stylizer : stylizer = default_stylizer) (term : M.t) : string =
    format ~stylizer (M.elaborate term) (module Fil)
  ;;
end
