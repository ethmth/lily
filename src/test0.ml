open Astscanner

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parserscanner.program Tokenize.tokenize lexbuf in
  print_endline (string_of_program program)
