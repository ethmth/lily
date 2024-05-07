(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/ (OUTDATED)
   http://llvm.moe/ocaml/ (OUTDATED)
   https://ocaml.org/p/llvm/16.0.6%2Bnnp/doc/Llvm/index.html

*)

open Libparser
open Libsemant
module L = Llvm
module A = Ast
open Sast
open Modules

module StringMap = Map.Make(String)
module FuncMap = Map.Make(FuncId)

(* translate : Sast.program -> Llvm.module *)
let translate (functions) (globals) =
  let context    = L.global_context () in

  (* Get types from the context *)
  let i64_t      = L.i64_type    context
  and i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i64_t
    | A.Bool  -> i1_t
    | A.Char  -> i8_t
    | A.Float -> float_t
    | A.Void  -> i1_t
  in
  let ltypes_of_typs (l:A.typ list): L.lltype list =
    List.map ltype_of_typ l
  in
  let type_of_sexpr (l:sexpr): A.typ = 
    match l with (t, _) -> t
  in
  let types_of_sexprs (l: sexpr list): A.typ list =
    List.map type_of_sexpr l
  in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "LILY" in


  let build_block (b: sblock) =
    b
  in

  ignore(build_block program);
  the_module
