module Color = struct
  (** [Color] encodes ANSI colors.
  
  They come in three flavors:
  - 3-bit ([Color8])
  - 8-bit ([Color256])
  - 24-bit ([Rgb]) *)

  module Color8 = struct
    (** [Color8] specifically encodes 3-bit ANSI colors. *)

    type t =
      | Black
      | Red
      | Green
      | Yellow
      | Blue
      | Magenta
      | Cyan
      | White

    (** [to_int color] produces the corresponding ANSI code of
        the [color], minus 30. *)
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

  (** [to_ansi color] converts the [color] into an embeddable
      portion of a SGR escape sequence. *)
  let to_ansi : t -> string = function
    | Color8 color -> Printf.sprintf "3%d" (Color8.to_int color)
    | Color256 color -> Printf.sprintf "38;5;%d" color
    | Rgb (r, g, b) -> Printf.sprintf "38;2;%d;%d;%d" r g b
  ;;
end

module Decoration = struct
  (** [Decoration] encodes the text decoration such as bold or
      italic. *)

  type t =
    | Bold
    | Dim
    | Italic
    | Underlined

  (** [to_int decoration] produces the corresponding ANSI code of the
      [decoration]. *)
  let to_int : t -> int = function
    | Bold -> 1
    | Dim -> 2
    | Italic -> 3
    | Underlined -> 4
  ;;
end

module Style = struct
  (** [Style] encodes terminal styling as an optional color and
      a list of text decorations. *)

  type t =
    { color : Color.t option
    ; decorations : Decoration.t list
    }

  (** [create ?color ?decorations ()] builds a new style object
      given an optionally provided [color] and list of
      [decorations]. *)
  let create ?(color : Color.t option) ?(decorations : Decoration.t list = []) () : t =
    { color; decorations }
  ;;

  (** Empty styling. *)
  let none = { color = None; decorations = [] }

  (** [to_ansi style] produces a fully usable ANSI escape
      sequence from the [style]. *)
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

module Token_type = struct
  (** [Token_type] is a (tentatively) language-agnostic that
      classifies the tokens for formatting granularity. *)

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

type stylizer = Token_type.t -> Style.t

module Token = struct
  (** [Token] is the formatter token data type. *)

  (** A token is simply a pair (token type, lexeme). *)
  type t = Token_type.t * string

  (** Token representing a single whitespace.
      Exists as a constant due to its recurrent use. *)
  let space : t = Token_type.Space, " "

  (** [format stylizer token] formats [token] into a printable
      string. *)
  let format (stylizer : stylizer) : t -> string = function
    | token_type, lexeme ->
      Printf.sprintf "%s%s\x1b[0m" (Style.to_ansi (stylizer token_type)) lexeme
  ;;
end

module type TOKENIZABLE = sig
  (** [TOKENIZABLE] is the interface for languages which terms
      can be transformed into a stream of formatter tokens. *)

  (** [t] encodes the language. *)
  type t

  (** [tokenize term] transforms [term] into a stream of
      formatter tokens. *)
  val tokenize : t -> Token.t list
end

(** Default stylizer for the formatter. *)
let default_stylizer : stylizer =
  let open Token_type in
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

(** [format ?stylizer term (module M)] formats the [term] using
    the [stylizer] if [M] provides tokenization for the [term]
    type. *)
let format
      (type t)
      ?(stylizer : stylizer = default_stylizer)
      (tree : t)
      (module M : TOKENIZABLE with type t = t)
  : string
  =
  tree |> M.tokenize |> List.map (Token.format stylizer) |> String.concat ""
;;
