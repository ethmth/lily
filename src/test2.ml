open Libparser
open Libsemant
open Sast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Tokenize.tokenize lexbuf in
  let (preprocessed_program) = Presemant.preprocess program in 
  ignore(print_endline (Ast.string_of_program preprocessed_program));
  let (sprogram, _, _) = Semant.check (Presemant.preprocess program) in
  print_endline (string_of_sprogram sprogram)
 