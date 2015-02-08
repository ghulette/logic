open Formula

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let _ = Parser.main Lexer.token lexbuf in
  print_endline "ok"; flush stdout
