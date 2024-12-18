open Grammar

let meta_next_id = ref 0

let make_fresh_meta () : Core.ty =
  let id = !meta_next_id in
  incr meta_next_id;
  Core.Meta_var id
;;

let rec expr_to_term : Expr.t -> Core.term = function
  | Expr.Identifier (_, lexeme, _) -> { ty = make_fresh_meta (); term' = Core.Var lexeme }
  | Expr.Nat_literal (_, lexeme, _) ->
    { ty = make_fresh_meta (); term' = Core.Lit (int_of_string lexeme) }
  | Expr.Grouping expr -> expr_to_term expr
  | Expr.Application { application; argument } ->
    { ty = make_fresh_meta ()
    ; term' = Core.App (expr_to_term application, expr_to_term argument)
    }
  | Expr.Function_literal { parameter = _, lexeme, _; body } ->
    { ty = make_fresh_meta ()
    ; term' = Core.Fun (lexeme, make_fresh_meta (), expr_to_term body)
    }
  | Expr.Let { name = _, lexeme, _; body } ->
    { ty = make_fresh_meta ()
    ; term' = Core.Let (lexeme, make_fresh_meta (), expr_to_term body)
    }
;;
