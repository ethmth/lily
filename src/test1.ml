open Lilylib
open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Tokenize.tokenize lexbuf in
  print_endline (string_of_program program)
