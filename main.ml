let _ =
  let lexbuf = Lexing.from_channel stdin in
  let fm = Parser.main Lexer.token lexbuf in
  let s = Formula.to_string fm in
  print_endline s;
  flush stdout
