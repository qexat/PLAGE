open Grammar
open Core

let make_fresh_metavariable =
  let generator = Id.get_generator () in
  fun () -> Type.Metavariable (generator ())
;;

let rec elaborate : Expr.t -> Term.t = function
  | Expr.Identifier (_, lexeme, _) -> Term.var (make_fresh_metavariable ()) lexeme
  | Expr.Nat_literal (_, lexeme, _) ->
    Core.Term.lit (make_fresh_metavariable ()) (Value.Nat (int_of_string lexeme))
  | Expr.Grouping expr -> elaborate expr
  | Expr.Application { application; argument } ->
    Term.app (make_fresh_metavariable ()) (elaborate application) (elaborate argument)
  | Expr.Function_literal { parameter = _, lexeme, _; body } ->
    Term.fun'
      (make_fresh_metavariable ())
      lexeme
      (make_fresh_metavariable ())
      (elaborate body)
  | Expr.Let { name = _, lexeme, _; body } ->
    Term.let'
      (make_fresh_metavariable ())
      lexeme
      (make_fresh_metavariable ())
      (elaborate body)
;;
