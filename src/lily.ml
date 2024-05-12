(* Top-level of the LILY compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

open Libparser
open Libsemant
open Libirgen
open Readers
type action = Ast | Sast | LLVM_IR

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
  ] in
  let usage_msg = "usage: ./lily.native [-a|-s|-l] [file.mc]" in
  (* let channel = ref stdin in *)
  (* Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg; *)
  
  let program_string = ref "" in
  Arg.parse speclist (fun filename -> program_string := (get_program_string (Some(filename)))) usage_msg;
  if !program_string = "" then (program_string := (get_program_string (None))) else
  ignore(!program_string);

  (* let lexbuf = Lexing.from_channel !channel in *)
  let lexbuf = Lexing.from_string !program_string in

  let ast = Parser.program Tokenize.tokenize lexbuf in
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | _ -> let (sast, globals, functions) = Semant.check (Presemant.preprocess ast) in
    match !action with
      Ast     -> ()
    | Sast    -> print_string (Sast.string_of_sprogram sast)
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate (globals,functions)))
