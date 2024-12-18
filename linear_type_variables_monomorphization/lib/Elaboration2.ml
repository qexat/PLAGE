open Grammar2

let make_fresh_metavariable () =
  let generator = Core2.Id.get_generator () in
  Core2.Type.Metavariable (generator ())
;;

let rec elaborate : Expr.t -> Core2.Term.t = function
  | Expr.Identifier (_, lexeme, _) -> Core2.Term.var (make_fresh_metavariable ()) lexeme
  | Expr.Nat_literal (_, lexeme, _) ->
    Core2.Term.lit (make_fresh_metavariable ()) (Core2.Value.Nat (int_of_string lexeme))
  | Expr.Grouping expr -> elaborate expr
  | Expr.Application { application; argument } ->
    Core2.Term.app
      (make_fresh_metavariable ())
      (elaborate application)
      (elaborate argument)
  | Expr.Function_literal { parameter = _, lexeme, _; body } ->
    Core2.Term.fun'
      (make_fresh_metavariable ())
      lexeme
      (make_fresh_metavariable ())
      (elaborate body)
  | Expr.Let { name = _, lexeme, _; body } ->
    Core2.Term.let'
      (make_fresh_metavariable ())
      lexeme
      (make_fresh_metavariable ())
      (elaborate body)
;;
