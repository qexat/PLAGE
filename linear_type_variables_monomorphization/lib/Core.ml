type name = string
type value = int
type type_var_id = int
type meta_var_id = int

type prim =
  | Nat_type
  | Unit_type

type ty =
  | Prim_type of prim
  | Fun_type of ty * ty
  | Meta_var of meta_var_id

type 'a untyped_term =
  | App of 'a * 'a
  | Fun of name * ty * 'a
  | Let of name * ty * 'a
  | Lit of value
  | Var of name

type term =
  { ty : ty
  ; term' : term untyped_term
  }

module Highlighting = struct
  let keyword : string -> string = Printf.sprintf "\x1b[1;35m%s\x1b[22;39m"
  let operator : string -> string = Printf.sprintf "\x1b[35m%s\x1b[39m"
  let ty : string -> string = Printf.sprintf "\x1b[33m%s\x1b[39m"
  let identifier : string -> string = Printf.sprintf "\x1b[36m%s\x1b[39m"
  let number : int -> string = Printf.sprintf "\x1b[38;2;153;51;204m%d\x1b[39m"
  let comment : string -> string = Printf.sprintf "\x1b[2m%s\x1b[22m"
  let type_anno : string -> string = Printf.sprintf "\x1b[2m: %s\x1b[22m"
end

let prim_to_string (prim : prim) : string =
  (match prim with
   | Nat_type -> "nat"
   | Unit_type -> "unit")
  |> Highlighting.ty
;;

let rec ty_to_string : ty -> string = function
  | Prim_type prim -> prim_to_string prim
  | Fun_type (left, right) ->
    Printf.sprintf "(%s) -> %s" (ty_to_string left) (ty_to_string right)
  | Meta_var id -> Printf.sprintf "%s" (Highlighting.ty (Printf.sprintf "?%d" id))
;;

let untyped_term_to_string (converter : 'a -> string) : 'a untyped_term -> string
  = function
  | App (l, r) -> Printf.sprintf "%s %s" (converter l) (converter r)
  | Fun (param, ty, body) ->
    Printf.sprintf
      "%s (%s %s) %s %s"
      (Highlighting.keyword "fun")
      (Highlighting.identifier param)
      (Highlighting.type_anno (ty_to_string ty))
      (Highlighting.operator "->")
      (converter body)
  | Let (identifier, ty, body) ->
    Printf.sprintf
      "%s %s %s %s %s"
      (Highlighting.keyword "let")
      (Highlighting.identifier identifier)
      (Highlighting.type_anno (ty_to_string ty))
      (Highlighting.operator "=")
      (converter body)
  | Lit value -> Highlighting.number value
  | Var name -> Highlighting.identifier name
;;

let rec typed_term_to_string ?(show_anno : bool = true) (term : term) : string =
  let aux (ty : ty) (show_anno : bool) : string -> string =
    match show_anno with
    | true ->
      fun term -> Printf.sprintf "%s %s" term (Highlighting.type_anno (ty_to_string ty))
    | false -> Printf.sprintf "%s"
  in
  let { ty; term' } = term in
  aux ty show_anno (untyped_term_to_string (typed_term_to_string ~show_anno) term')
;;
