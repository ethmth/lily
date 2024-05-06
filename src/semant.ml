(* Semantic checking for the LILY compiler *)

open Libparser
open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (program_block) =

  let check_block (block: block): sblock =
    print_endline "Hi";



    let empty_block (_unused_block: block): sblock = 
      SBlock([])
    in
    empty_block block
  in

  check_block program_block