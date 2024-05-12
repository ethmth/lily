open Libparser
open Libsemant
open Sast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Tokenize.tokenize lexbuf in
  let (sprogram, _, _) = Semant.check (Presemant.expand_fdecls program) in
  print_endline (string_of_sprogram sprogram)
