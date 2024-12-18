module Color = struct
  module Color8 = struct
    type t =
      | Black
      | Red
      | Green
      | Yellow
      | Blue
      | Magenta
      | Cyan
      | White

    let to_int : t -> int = function
      | Black -> 0
      | Red -> 1
      | Green -> 2
      | Yellow -> 3
      | Blue -> 4
      | Magenta -> 5
      | Cyan -> 6
      | White -> 7
    ;;
  end

  type t =
    | Color8 of Color8.t
    | Color256 of int
    | Rgb of int * int * int

  let black : t = Color8 Color8.Black
  let red : t = Color8 Color8.Red
  let green : t = Color8 Color8.Green
  let yellow : t = Color8 Color8.Yellow
  let blue : t = Color8 Color8.Blue
  let magenta : t = Color8 Color8.Magenta
  let cyan : t = Color8 Color8.Cyan
  let white : t = Color8 Color8.White

  let to_ansi : t -> string = function
    | Color8 color -> Printf.sprintf "3%d" (Color8.to_int color)
    | Color256 color -> Printf.sprintf "38;5;%d" color
    | Rgb (r, g, b) -> Printf.sprintf "38;2;%d;%d;%d" r g b
  ;;
end

module Decoration = struct
  type t =
    | Bold
    | Dim
    | Italic
    | Underlined

  let to_int : t -> int = function
    | Bold -> 1
    | Dim -> 2
    | Italic -> 3
    | Underlined -> 4
  ;;
end

module Style = struct
  type t =
    { color : Color.t option
    ; decorations : Decoration.t list
    }

  let create ?(color : Color.t option) ?(decorations : Decoration.t list = []) () : t =
    { color; decorations }
  ;;

  let none = { color = None; decorations = [] }

  let decoration_list_to_ansi (decorations : Decoration.t list) : string =
    decorations
    |> List.map Decoration.to_int
    |> List.map Int.to_string
    |> String.concat ";"
  ;;

  let to_ansi ({ color; decorations } : t) : string =
    let color_ansi =
      color
      |> Option.map Color.to_ansi
      |> Option.map (( ^ ) ";")
      |> Option.value ~default:""
    in
    let decorations_ansi = decoration_list_to_ansi decorations in
    Printf.sprintf "\x1b[%sm" decorations_ansi ^ color_ansi
  ;;
end

module Token = struct
  module Type = struct
    type t =
      | Comment
      | Documentation
      | Keyword
      | Identifier
      | Constant
      | Function
      | Parameter
      | Type
      | Trait
      | Module
      | Macro
      | Special
      | Operator_statement
      | Operator_expression
      | Operator_type
      | String
      | Constant_literal
      | Strong_punctuation
      | Weak_punctuation
      | Space
      | Indent
  end

  type t = Type.t * string
  type highlighter = Type.t -> Style.t

  let format (highlighter : highlighter) : t -> string = function
    | token_type, lexeme ->
      Printf.sprintf "%s%s\x1b[0m" (Style.to_ansi (highlighter token_type)) lexeme
  ;;
end

module type TOKENIZABLE = sig
  type t

  val tokenize : t -> Token.t list
end

module Tree = struct
  type t =
    | Assignment of t * t
    | Constant_literal of string
    | Function_literal of t * t
    | Identifier of string
    | Parameter_name of string
    | String_literal of string
    | Type_annotated of t * string

  let rec tokenize : t -> Token.t list = function
    | Assignment (name, body) ->
      [ Token.Type.Keyword, "let"; Token.Type.Space, " " ]
      @ tokenize name
      @ [ Token.Type.Space, " "
        ; Token.Type.Operator_statement, "="
        ; Token.Type.Space, " "
        ]
      @ tokenize body
    | Constant_literal lexeme -> [ Token.Type.Constant_literal, lexeme ]
    | Function_literal (param, body) ->
      [ Token.Type.Keyword, "fun"; Token.Type.Space, " " ]
      @ tokenize param
      @ [ Token.Type.Space, " "
        ; Token.Type.Operator_statement, "->"
        ; Token.Type.Space, " "
        ]
      @ tokenize body
    | Identifier lexeme -> [ Token.Type.Identifier, lexeme ]
    | Parameter_name lexeme -> [ Token.Type.Parameter, lexeme ]
    | String_literal lexeme -> [ Token.Type.String, lexeme ]
    | Type_annotated (expr, lexeme) ->
      [ Token.Type.Strong_punctuation, "(" ]
      @ tokenize expr
      @ [ Token.Type.Space, " "
        ; Token.Type.Strong_punctuation, ":"
        ; Token.Type.Space, " "
        ; Token.Type.Type, lexeme
        ; Token.Type.Strong_punctuation, ")"
        ]
  ;;
end

type highlighter = Token.highlighter

let default_highlighter : highlighter =
  let open Token.Type in
  function
  | Comment | Documentation -> Style.create ~decorations:[ Decoration.Dim ] ()
  | Keyword | Operator_statement | Operator_type -> Style.create ~color:Color.magenta ()
  | Identifier -> Style.create ~color:Color.cyan ()
  | Parameter -> Style.create ~color:Color.cyan ~decorations:[ Decoration.Italic ] ()
  | Constant | Constant_literal -> Style.create ~color:(Color.Rgb (153, 51, 204)) ()
  | Function | Operator_expression -> Style.create ~color:Color.blue ()
  | Type | Trait | Module -> Style.create ~color:Color.yellow ()
  | Macro | Special -> Style.create ~color:Color.red ()
  | String -> Style.create ~color:Color.green ()
  | _ -> Style.none
;;

let highlight
      (type t)
      ?(highlighter : highlighter = default_highlighter)
      (tree : t)
      (module M : TOKENIZABLE with type t = t)
  : string
  =
  tree |> M.tokenize |> List.map (Token.format highlighter) |> String.concat ""
;;

let highlight_tree ?(highlighter : highlighter = default_highlighter) (tree : Tree.t) =
  highlight ~highlighter tree (module Tree)
;;
