open Astscanner

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parserscanner.program Scanner.token lexbuf in
  print_endline (string_of_program program)
