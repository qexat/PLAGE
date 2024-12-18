module Name = struct
  type t = string
end

module Value = struct
  type t = Nat of int

  let to_string : t -> string = function
    | Nat value -> Int.to_string value
  ;;
end

module Id = struct
  type t = int

  let get_generator ?(start : t = 0) () : unit -> t =
    let next = ref start in
    fun () ->
      let id = !next in
      incr next;
      id
  ;;

  let to_string : t -> string = Int.to_string
end

module Type = struct
  module Primitive = struct
    type t =
      | Nat_type
      | Unit_type

    let to_string : t -> string = function
      | Nat_type -> "nat"
      | Unit_type -> "unit"
    ;;
  end

  type t =
    | Primitive_type of Primitive.t
    | Function_type of t * t
    | Metavariable of Id.t

  let nat = Primitive_type Primitive.Nat_type
  let unit = Primitive_type Primitive.Unit_type

  let rec to_string : t -> string = function
    | Primitive_type prim -> Primitive.to_string prim
    | Function_type (left, right) ->
      Printf.sprintf "(%s) -> %s" (to_string left) (to_string right)
    | Metavariable id -> Printf.sprintf "?%s" (Id.to_string id)
  ;;

  let rec elaborate : t -> Format.Fil.t = function
    | Primitive_type prim -> Format.Fil.Type_identifier (Primitive.to_string prim)
    | Function_type (left, right) ->
      Format.Fil.Infix_stmt (elaborate left, "->", elaborate right)
    | Metavariable id ->
      Format.Fil.Type_identifier (Printf.sprintf "?%s" (Id.to_string id))
  ;;

  module Notation = struct
    let ( --> ) (left : t) (right : t) : t = Function_type (left, right)
  end
end

module TypeFormatter = Format.MakeFormatter (Type)

module Term = struct
  module Untyped = struct
    type ('ty, 'self) t =
      | App of 'self * 'self
      | Fun of Name.t * 'ty * 'self
      | Let of Name.t * 'ty * 'self
      | Lit of Value.t
      | Var of Name.t

    let elaborate
          ~(type_renderer : 'ty -> string)
          ~(self_elaborator : 'self -> Format.Fil.t)
      : (_, 'self) t -> Format.Fil.t
      = function
      | App (application, argument) ->
        Format.Fil.Application (self_elaborator application, self_elaborator argument)
      | Fun (param, ty, body) ->
        Format.Fil.Function_literal
          (Format.Fil.Parameter (param, type_renderer ty), self_elaborator body)
      | Let (name, ty, body) ->
        Format.Fil.Assignment
          ( Format.Fil.Type_annotated (Format.Fil.Identifier name, type_renderer ty)
          , self_elaborator body )
      | Lit value -> Format.Fil.Constant_literal (Value.to_string value)
      | Var name -> Format.Fil.Identifier name
    ;;
  end

  type t =
    { ty : Type.t
    ; term : (Type.t, t) Untyped.t
    }

  let app (ty : Type.t) (application : t) (argument : t) : t =
    { ty; term = Untyped.App (application, argument) }
  ;;

  let fun' (ty : Type.t) (param : Name.t) (param_type : Type.t) (body : t) : t =
    { ty; term = Untyped.Fun (param, param_type, body) }
  ;;

  let let' (ty : Type.t) (name : Name.t) (name_type : Type.t) (body : t) : t =
    { ty; term = Untyped.Let (name, name_type, body) }
  ;;

  let lit (ty : Type.t) (value : Value.t) : t = { ty; term = Untyped.Lit value }
  let var (ty : Type.t) (name : Name.t) : t = { ty; term = Untyped.Var name }

  let rec elaborate ({ ty; term } : t) : Format.Fil.t =
    Format.Fil.Type_annotated
      ( Untyped.elaborate
          ~type_renderer:TypeFormatter.format
          ~self_elaborator:elaborate
          term
      , TypeFormatter.format ty )
  ;;
end

module TermFormatter = Format.MakeFormatter (Term)
