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

  let to_ansi ({ color; decorations } : t) : string =
    let color_ansi = color |> Option.map Color.to_ansi |> Option.value ~default:"" in
    let decorations_ansi_list =
      decorations |> List.map Decoration.to_int |> List.map Int.to_string
    in
    let ansi_list = color_ansi :: decorations_ansi_list in
    Printf.sprintf
      "\x1b[%sm"
      (ansi_list |> List.find_all (fun s -> s <> "") |> String.concat ";")
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

  let space : t = Type.Space, " "

  let format (highlighter : Type.t -> Style.t) : t -> string = function
    | token_type, lexeme ->
      Printf.sprintf "%s%s\x1b[0m" (Style.to_ansi (highlighter token_type)) lexeme
  ;;
end

type stylizer = Token.Type.t -> Style.t

module type TOKENIZABLE = sig
  type t

  val tokenize : t -> Token.t list
end

module MakeFormattable (M : TOKENIZABLE) = struct
  include M

  let format ~(stylizer : stylizer) (term : t) : string =
    term |> tokenize |> List.map (Token.format stylizer) |> String.concat ""
  ;;
end

let default_stylizer : stylizer =
  let open Token.Type in
  function
  | Comment | Documentation -> Style.create ~decorations:[ Decoration.Dim ] ()
  | Keyword -> Style.create ~color:Color.magenta ~decorations:[ Decoration.Bold ] ()
  | Operator_statement | Operator_type -> Style.create ~color:Color.magenta ()
  | Identifier -> Style.create ~color:Color.cyan ()
  | Parameter -> Style.create ~color:Color.cyan ~decorations:[ Decoration.Italic ] ()
  | Constant | Constant_literal -> Style.create ~color:(Color.Rgb (153, 51, 204)) ()
  | Function | Operator_expression -> Style.create ~color:Color.blue ()
  | Type | Trait | Module -> Style.create ~color:Color.yellow ()
  | Macro | Special -> Style.create ~color:Color.red ()
  | String -> Style.create ~color:Color.green ()
  | _ -> Style.none
;;

let format
      (type t)
      ?(stylizer : stylizer = default_stylizer)
      (tree : t)
      (module M : TOKENIZABLE with type t = t)
  : string
  =
  tree |> M.tokenize |> List.map (Token.format stylizer) |> String.concat ""
;;
