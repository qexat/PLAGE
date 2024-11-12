let source = "3 + 5"
let lexer = new Ocalc.Lexer.lexer source
let tokens = lexer#tokenize ()
let parser = Ocalc.Parser.create tokens

let ast =
  match Ocalc.Parser.parse parser with
  | None ->
    Printf.printf "%d\n" parser.position;
    failwith "failed parsing"
  | Some ast -> ast
;;

let () = Ocalc.Ast_printer.print ast
