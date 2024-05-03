(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

open Libparser
open Libsemant
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context = L.global_context () in

  let the_module = L.create_module context "MicroC" in

  let i32_t = L.i32_type context in
  let i1_t = L.i1_type context in

  let ltype_of_type = function
    A.Int -> i32_t
    | A.Bool -> i1_t
  in 

  let global_vars: L.llvalue StringMap.t = 
    let global_var m (t, n) = 
      let init = L.const_int (ltype_of_type t) 0 
      in StringMap.add n (L.define_global n init the_module) m in
      List.fold_left global_var StringMap.empty globals
  in

  let ltype_of_bind (t, _) = ltype_of_type t in

  let function_decls: (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl = 
      let name = fdecl.sfname in
      let formal_types =  Array.of_list (List.map ltype_of_bind fdecl.sformals) in
      let ftype = L.function_type (ltype_of_type fdecl.srtyp) formal_types in 
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in 
    List.fold_left function_decl StringMap.empty functions
  in

  let local_vars = 0 in
  let lookup n = try StringMap.find n local_vars
  with Not_found StringMap.find n global_vars in

  let build_expr builder ((_, e):sexpr) = 
  match e with
    SLiteral i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SId s -> L.build_load (lookup s) s builder
    | SAssign (s, e) -> let e_addr = build_expr builder e in
    ignore(L.build_store e_addr (lookup s) builder); e_addr
    | SBinop (e1, op, e2) ->
      let e1_addr = build_expr builder e1 in
      let e2_addr = build_expr builder e2 in
      let l_op = match op with
        A.Add -> L.build_add
        | A.Sub -> L.build_sub
        | A.And -> L.build_and
        | _ -> L.build_or
      in
      l_op e1_addr e2_addr "tmp" builder
    | SCall(f, args) ->
        let (fdef, fast) = StringMap.find f function_decls in
        let llargs = List.map (build_expr builder) (List.rev args) in
        L.build_call fdef (Array.of_list (List.rev llargs)) (f^"result")
  in 

  let build_function_body fdecl = 
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let add_terminal block terminal =
      match L.block_terminator block with
      Some _ -> ()
      | None -> ignore (terminal);

    let rec build_stmt builder st = function
     SBlock sl -> List.fold_left build_stmt builder sl
     | SExpr e -> ignore (build_expr builder e); builder
     | SReturn e -> ignore (L.build_ret (build_expr builder e) builder); builder
     | SIf (e, then_stmt, else_stmt) -> 
      (*
        E.code 
        
        then:
          then_stmt
          jmp end
        else:
          else_stmt
          jmp end
        end:
      *)
      let e_addr = build_expr builder e in
      let then_bb = L.append_block context "then" the_function in
      let else_bb = L.append_block context "else" the_function in
      let end_bb = L.append_block context "if_end" the_function in

      ignore(L.build_cond_br e_addr then_bb else_bb builder);

      ignore(build_stmt (L.builder_at_end context then_bb) then_stmt);
      add_terminal then_bb (L.build_br end_bb (L.builder_at_end context then_bb));

      ignore(build_stmt (L.builder_at_end context else_bb) else_stmt);
      add_terminal else_bb (L.build_br end_bb (L.builder_at_end context else_bb));

      L.builder_at_end context end_bb

    | SWhile(e, body) ->
      (*
      sdsdd
      <- builder
      jmp while
      
      while:
        e.code
        cond_br e.addr body end
    
      body:
        body_stmt
        jmp while
      
      end:
       *)
       let while_bb = L.append_block context "while" the_function in
       ignore(L.build_br while_bb builder)

       let e_addr = build_expr (L.builder_at_end context while_bb) e in 
       let body_bb = L.append_block context "body" the_function in
       let end_bb = L.append_block context "while_end" the_function in
       ignore(L.build_cond_br e_addr body_bb end_bb builder (L.builder_at_end context while_bb));

       ignore(build_stmt (L.builder_at_end context body_bb) body);
       add_terminal body_bb (L.build_br while_bb (L.builder_at_end context else_bb));

       L.builder_at_end context end_bb
    in

    let func_builder = build_stmt builder (SBlock fdecl.sbody) in
    add_terminal func_builder
  in

  List.iter build_function_body functions;
  the_module