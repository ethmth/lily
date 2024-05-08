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

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate ((globals: (A.typ * string * string) list), functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "LILY" in

  (* Get types from the context *)
  let i64_t      = L.i64_type    context
  (* and i32_t      = L.i32_type    context *)
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
  (* let type_of_bind (l:A.bind): A.typ = 
    match l with (t, _) -> t
  in
  let types_of_binds (l: A.bind list): A.typ list =
    List.map type_of_bind l
  in *)

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m ((t: A.typ), (_: string), (cname: string)) =
      let init = if t != A.Float then L.const_int (ltype_of_typ t) 0 else L.const_float (ltype_of_typ t) 0.0
      in StringMap.add cname (L.define_global cname init the_module) m in
    List.fold_left global_var StringMap.empty globals in
  ignore(global_vars);

  (* let printf_t : L.lltype =
    L.var_arg_function_type i64_t [| L.pointer_type context |] in
  let printf_func : L.llvalue =
    L.declare_function "print" printf_t the_module in *)

  let print_t : L.lltype =
    L.var_arg_function_type i64_t [| i64_t|] in
  let print_func : L.llvalue =
    L.declare_function "printf" print_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sstmt) StringMap.t =
    let function_decl m fdecl =
      match fdecl with 
      SFdecl(rtyp, _, args, _, cname) ->
      (* let name = fdecl.sfname *)
      let bind_from_args (arg: A.typ * string * string) =
        match arg with (t, _ ,cname) -> (t, cname)
      in
      let formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) (List.map bind_from_args args))
      in let ftype = L.function_type (ltype_of_typ rtyp) formal_types in
      StringMap.add cname (L.define_function cname ftype the_module, fdecl) m 
    | _ -> raise (Failure ("Unexpected statement in function_decls")) in
    List.fold_left function_decl StringMap.empty functions in
    ignore(function_decls);

  (* Fill in the body of the given function *)

  (* let build_block (sblock: sblock) (builder: L.llbuilder) = 
    let lookup n = 
      StringMap.find n global_vars
    in
  in *)


  let build_function_body fdecl =
    match fdecl with SFdecl(rtyp, _, _, sblock, cname) ->
    let (the_function, _) = StringMap.find cname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in


    (* TODO Assign args to global vars *)

    (* let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in *)

    (* let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in *)

    let lookup n = 
      try StringMap.find n global_vars with Not_found -> raise (Failure ("lookup failure"))
    in

    let rec build_expr builder ((t, e) : sexpr) = match e with
        SLitInt i  -> L.const_int (ltype_of_typ A.Int) i
      | SLitBool b -> L.const_int (ltype_of_typ A.Bool) (if b then 1 else 0)
      (* | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0) *)
      | SLitChar c -> L.const_int (ltype_of_typ A.Char) (int_of_char c)
      | SLitFloat f -> L.const_float (ltype_of_typ A.Float) f
      | SId (_, cname) -> L.build_load (ltype_of_typ t) (lookup cname) cname builder
      (* | SAssign (s, e, cname) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup cname) builder); e' *)
      | SBinop (e1, o, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match o with
           A.Plus    ->  L.build_add
         | A.Minus     -> L.build_sub
         | A.Times    -> L.build_mul
         | A.Divide   -> L.build_sdiv
         (* | A.And     -> L.build_and *)
         (* | A.Or      -> L.build_or *)
         (* TODO: Make these fcmp when its floats *)
         | A.Eq   -> L.build_icmp L.Icmp.Eq
         | A.Neq   -> L.build_icmp L.Icmp.Ne
         | A.Lt    -> L.build_icmp L.Icmp.Slt
         (* TODO: All of these are Less Thans rn *)
         | A.Leq    -> L.build_icmp L.Icmp.Slt
         | A.Gt     -> L.build_icmp L.Icmp.Slt
         | A.Geq    -> L.build_icmp L.Icmp.Slt
        ) e1' e2' "tmp" builder
      (* | SCall ("print", [e]) ->
        let func_type = L.function_type (ltype_of_typ A.Int) (Array.of_list ([(ltype_of_typ (type_of_sexpr e))])) in 
        L.build_call func_type printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder *)
      | SUnaryOp (o, e) ->
        let e' = build_expr builder e in 
        (match o with
           A.Negate    ->  L.build_neg
        ) e' "tmp" builder
      | SCall ("print", [e], _) ->
          let func_type = L.function_type (ltype_of_typ A.Int) (Array.of_list ([(ltype_of_typ (type_of_sexpr e))])) in 
          L.build_call func_type print_func [| int_format_str ; (build_expr builder e) |]
            "printf" builder
      | SCall (_, args, cname) ->
        let (fdef, _) = try StringMap.find cname function_decls with Not_found -> raise (Failure ("SCall function " ^ cname ^ "not found.")) in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = cname ^ "_result" in
        let arg_types = ltypes_of_typs (types_of_sexprs args) in
        let func_type = L.function_type (ltype_of_typ t) (Array.of_list arg_types) in 
        L.build_call func_type fdef (Array.of_list llargs) result builder
    in

    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in
    
    let rec build_stmt builder = function
        (* SBlock sl -> List.fold_left build_stmt builder sl *)
      | SExprStmt e -> ignore(build_expr builder e); builder
      | SAssign (_, e, cname) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup cname) builder); builder
      | SDeclAssign(_, _, e, cname) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup cname) builder); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | SIf (predicate, then_block, else_block) ->
        let then_stmt_list = match then_block with SBlock(then_stmt) -> then_stmt in
        let else_stmt_list = match else_block with SBlock(else_stmt) -> else_stmt in

        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (List.fold_left build_stmt (L.builder_at_end context then_bb) then_stmt_list);
        let else_bb = L.append_block context "else" the_function in
        ignore (List.fold_left build_stmt (L.builder_at_end context else_bb) else_stmt_list);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let body_stmt_list = match body with SBlock(sl) -> sl in
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        (* add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while; *)
        add_terminal (List.fold_left build_stmt builder body_stmt_list) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb
        (* TODO: Implement For Loops *)
      | SFor (_, _, _) -> builder
      | SDecl(_, _, _) -> builder
      | SFdecl(_, _, _, _, _) -> builder
      (* | _ -> builder; *)

    in
    (* let func_builder = build_stmt builder (SBlock fdecl.sbody) in *)
    let sl = match sblock with SBlock(sl) -> sl in
    let func_builder = List.fold_left build_stmt builder sl in
    add_terminal func_builder (L.build_ret ((if rtyp == A.Float then (L.const_float (ltype_of_typ rtyp) 0.0) else L.const_int (ltype_of_typ rtyp) 0)))

    | _ -> raise (Failure("Unexpected statement passed to build_function_body"))
  in

  List.iter build_function_body functions;
  the_module