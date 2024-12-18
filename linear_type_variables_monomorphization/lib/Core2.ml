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
    | Metavariable id -> Printf.sprintf "%s" (Id.to_string id)
  ;;

  module Notation = struct
    let ( --> ) (left : t) (right : t) : t = Function_type (left, right)
  end
end

module Term = struct
  module Untyped = struct
    type ('ty, 'self) t =
      | App of 'self * 'self
      | Fun of Name.t * 'ty * 'self
      | Let of Name.t * 'ty * 'self
      | Lit of Value.t
      | Var of Name.t

    let to_string ~(type_fmt : 'ty -> string) ~(self_fmt : 'self -> string)
      : ('ty, 'self) t -> string
      = function
      | App (l, r) -> Printf.sprintf "%s %s" (self_fmt l) (self_fmt r)
      | Fun (param, ty, body) ->
        Printf.sprintf "fun (%s : %s) -> %s" param (type_fmt ty) (self_fmt body)
      | Let (name, ty, body) ->
        Printf.sprintf "let %s : %s = %s" name (type_fmt ty) (self_fmt body)
      | Lit value -> Value.to_string value
      | Var name -> name
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

  let rec to_string ?(show_anno : bool = true) (term : t) : string =
    let aux (ty : Type.t) (show_anno : bool) : string -> string =
      match show_anno with
      | false -> Printf.sprintf "%s"
      | true -> fun term -> Printf.sprintf "%s : %s" term (Type.to_string ty)
    in
    let { ty; term } = term in
    aux
      ty
      show_anno
      (Untyped.to_string ~type_fmt:Type.to_string ~self_fmt:(to_string ~show_anno) term)
  ;;
end
