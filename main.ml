let _ =
  let lexbuf = Lexing.from_channel stdin in
  let e = Parser.main Lexer.token lexbuf in
  Formula.print e; flush stdout
