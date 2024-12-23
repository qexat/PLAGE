(* This implementation is a bit weird.
   What is called "Environment" is actually a wrapper around ε and Γ. *)
type t =
  { hypotheses : Hypotheses.t (* the actual ε *)
  ; context : Context.t (* Γ, optimized *)
  }

(** The empty environment. *)
let empty = { hypotheses = []; context = [] }

(** [create hypotheses] produces an environment containing [hypotheses]. *)
let create (hypotheses : Hypotheses.t) : t =
  { hypotheses; context = Context.of_hypotheses hypotheses }
;;

(* To determine whether a variable is defined in the current
   environment, we would query Γ and see if it is a member of
   the resulting set. But since Γ is implemented as a list
   directly, [is_defined] becomes a mere wrapper around
   [List.mem].*)
(** [is_defined environment name] determines whether [name] is defined in [environment] *)
let is_defined (environment : t) (name : string) : bool =
  List.mem name environment.context
;;

(** [define_variable environment name] defines a single variable [name] in the [environment] *)
let define_variable (environment : t) (name : string) : t =
  let variable = Term.TVar name in
  let prop = Proposition.Defined variable in
  { hypotheses = environment.hypotheses @ [ prop ]
  ; context = environment.context @ [ name ]
  }
;;

(** [define_variables environment names] defines each [names] in the [environment] *)
let rec define_variables (environment : t) (names : string list) : t =
  match names with
  | [] -> environment
  | head :: tail -> define_variables (define_variable environment head) tail
;;

(** [add_type_constraint] adds the proposition [name] : [type_name] to the [environment] *)
let add_type_constraint (environment : t) (name : string) (type_name : string) : t =
  let prop = Proposition.TypeOf (Term.TVar name, Term.TVar type_name) in
  { hypotheses = environment.hypotheses @ [ prop ]; context = environment.context }
;;

(** [show environment] produces a printable view of the [environment] *)
let show : t -> string = function
  | { hypotheses; context } -> Hypotheses.show hypotheses ^ "\n" ^ Context.show context
;;
