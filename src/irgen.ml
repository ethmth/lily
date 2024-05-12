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

let translate ((globals: (A.typ * string * string) list), (functions: sstmt list)) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "LILY" in

  let list_start_offset = 32 in

  (* Get types from the context *)
  let i64_t      = L.i64_type    context
  and _       = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t           = L.i1_type     context
  and float_t    = L.double_type context
  and ptr_t      = L.pointer_type context
  and byte_t       = L.i8_type     context
  in

  let typ_to_id(t: A.typ): int =
    match t with
      A.Int   -> 1
    | A.Bool  -> 2
    | A.Char  -> 3
    | A.Float -> 4
    | A.Void  -> 5
    | A.List(_) -> 6
    | A.Any -> raise (Failure("IR Error (typ_to_id): attempting to allocate memory for Any type"))
  in

  let size_of_typ(t:A.typ): int =
    match t with
      A.Int   -> 8
    | A.Bool  -> 1
    | A.Char  -> 1
    | A.Float -> 8
    | A.Void  -> raise (Failure("IR Error (size_of_typ): attempting to allocate memory for Void type"))
    | A.List(_) -> raise (Failure("IR Error (size_of_typ): attempting to allocate memory for List type"))
    | A.Any -> raise (Failure("IR Error (size_of_typ): attempting to allocate memory for Any type"))
  in

  (* Return the LLVM type for a LILY type *)
  let ltype_of_typ = function
      A.Int   -> i64_t
    | A.Bool  -> i1_t
    | A.Char  -> i8_t
    | A.Float -> float_t
    | A.Void  -> i8_t
    | A.List(_) -> ptr_t
    | A.Any -> raise (Failure("IR Error (ltype_of_typ): attempting to allocate memory for Any type"))
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

  let error_handler =
    let builder = L.builder context in
    let fatal_error_handler( f: string):unit =
      let printf_t : L.lltype =
        L.var_arg_function_type (ltype_of_typ A.Int) [| L.pointer_type context |] in
      let printf_func : L.llvalue =
        L.declare_function "printf" printf_t the_module in 
      let func_type = L.function_type (ltype_of_typ A.Int) (Array.of_list []) in
      let fmt_str = L.build_global_stringptr f "fmt" builder in
      ignore(L.build_call func_type printf_func (Array.of_list ([fmt_str]))
        "printf" builder);
    in
    L.install_fatal_error_handler fatal_error_handler
  in
  ignore(error_handler);

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m ((t: A.typ), (_: string), (cname: string)) =
      match t with
      A.Float -> let init = L.const_float (ltype_of_typ t) 0.0 in StringMap.add cname (L.define_global cname init the_module) m
      | A.List(_) -> let init = L.const_pointer_null ptr_t in StringMap.add cname (L.define_global (cname) init the_module) m
      | _ -> let init= L.const_int (ltype_of_typ t) 0 in StringMap.add cname (L.define_global cname init the_module) m
    in
    List.fold_left global_var StringMap.empty globals in
  ignore(global_vars);

  let malloc_count = ref 0 in
  let malloc_list: (L.llvalue StringMap.t) ref = ref StringMap.empty in

  let function_decls : (L.llvalue * sstmt) StringMap.t =
    let function_decl m fdecl =
      match fdecl with 
      SFdecl(rtyp, _, args, _, cname) ->
      let bind_from_args (arg: A.typ * string * string) =
        match arg with (t, _ ,cname) -> (t, cname)
      in
      let formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) (List.map bind_from_args args))
      in let ftype = L.function_type (ltype_of_typ rtyp) formal_types in
      StringMap.add cname (L.define_function cname ftype the_module, fdecl) m 
    | _ -> raise (Failure ("IR Error (function_decls): Unexpected non-Function statement ")) in
    List.fold_left function_decl StringMap.empty functions in
    ignore(function_decls);

  let build_function_body fdecl =
    match fdecl with SFdecl(rtyp, _, args, sblock, cname) ->
    let (the_function, _) = try StringMap.find cname function_decls with Not_found -> raise (Failure("IR Error (build_function_body): function " ^ cname ^ " not found.")) in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let lookup n = 
      try StringMap.find n global_vars with Not_found -> raise (Failure ("IR Error (lookup): lookup failure"))
    in

    (* Assign parameters passed in to their matching global vars *)
    let add_formal m (t, n, cname) p =
      L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder);
      ignore(L.build_store p (lookup cname) builder);
      StringMap.add n local m
    in
    let formals = List.fold_left2 add_formal StringMap.empty args
      (Array.to_list (L.params the_function)) in
    ignore(formals);

     (* Util/Built-in function definitions *)
     (* TODO: More print functions (those without spaces, without newlines, etc) *)
    let rec build_print_call (arg_list: sexpr list) (builder: L.llbuilder) (spacing: bool): L.llvalue =
      (* TODO: Good bool printing function; printing strings, lists, etc. *)
      let typ_to_fmt (t: A.typ): string =
        match t with
        | Int -> "%d"
        | Float -> "%f"
        | Char -> "%c"
        | Bool -> "%B"
        (* | String -> "%s" *)
        | _ -> ""
      in
      let rec get_format_str (typ_list: A.typ list): string =
        match typ_list with
        [] -> (if spacing then "\n" else "")
        | [last] -> typ_to_fmt last ^ (if spacing then "\n" else "")
        | h::t -> (typ_to_fmt h) ^ (if spacing then " " else "") ^ (get_format_str t)
      in 
      let get_typ (arg: sexpr) =
        match arg with (t, _) -> t
      in
      let rec build_expr_list (li) (builder): L.llvalue list = 
        match li with
        [] -> []
        | h::t -> [(build_expr builder h)] @ (build_expr_list t builder)
      in
      let typ_list = List.map get_typ arg_list in
      let ret_typ = List.hd typ_list in
      let ltyp_list = List.map ltype_of_typ typ_list in
      let format_str = get_format_str typ_list in
      let fmt_str = L.build_global_stringptr format_str "fmt" builder in
      let func_type = L.function_type (ltype_of_typ ret_typ) (Array.of_list (ltyp_list)) in
      let printf_t : L.lltype =
        L.var_arg_function_type (ltype_of_typ ret_typ) [| L.pointer_type context |] in
      let printf_func : L.llvalue =
        L.declare_function "printf" printf_t the_module in 
      let built_expr_list = build_expr_list arg_list builder in
      L.build_call func_type printf_func (Array.of_list ([fmt_str] @ built_expr_list))
        "printf" builder 


    and build_list_malloc (len: L.llvalue) (list_typ: A.typ): L.llvalue =
      let len_calc_offset = L.build_mul (L.const_int (ltype_of_typ A.Int) (size_of_typ list_typ) ) (len) "listindexmul" builder in
      ignore(len_calc_offset);
      let len_add_offset = L.build_add (len_calc_offset) (L.const_int (ltype_of_typ A.Int) list_start_offset) "listindexadd" builder in
      ignore(len_add_offset);
      let ptr = (L.build_array_malloc (byte_t) (len_add_offset) "listlitmalloc" builder) in
      let global_name = ("malvar!!!" ^ (string_of_int !malloc_count)) in 
      ignore(malloc_count := !malloc_count + 1);
      let global_malloc_var = (L.define_global (global_name) (L.const_pointer_null ptr_t) the_module) in
      ignore(malloc_list := (StringMap.add ("malvar!!!" ^ (string_of_int !malloc_count)) global_malloc_var !malloc_list));
      let global_assign = (L.build_store ptr global_malloc_var builder) in
      ignore(global_assign);
      ignore(ptr);
      let user_size_store = (L.build_store (len) ptr builder) in
      ignore(user_size_store);
      let size_offset = L.build_gep byte_t ptr (Array.of_list [(L.const_int byte_t 8)]) "listlittyp" builder in
      let size_store = L.build_store (len) size_offset builder in
      ignore(size_store);
      let typ_offset = L.build_gep byte_t ptr (Array.of_list [(L.const_int byte_t 16)]) "listlittyp" builder in
      let typ_store = L.build_store (L.const_int (ltype_of_typ A.Int) (typ_to_id list_typ)) typ_offset builder in
      ignore(typ_store);
      let byte_offset = L.build_gep byte_t ptr (Array.of_list [(L.const_int byte_t 24)]) "listlitbyte" builder in
      let byte_store = L.build_store (L.const_int (ltype_of_typ A.Int) (size_of_typ list_typ)) byte_offset builder in
      ignore(byte_store);
      ptr

    and build_expr (builder: L.llbuilder) ((t,e ): sexpr): L.llvalue = 
      match e with
        SAssign (_, e, cname) -> (
          match t with
          | _ -> (
          let e' = build_expr builder e in
          ignore(L.build_store e' (lookup cname) builder); e')
        )
      |  SLitInt i -> L.const_of_int64 (ltype_of_typ A.Int) (Int64.of_int i) (true)
      | SLitBool b -> L.const_int (ltype_of_typ A.Bool) (if b then 1 else 0)
      | SLitChar c -> L.const_int (ltype_of_typ A.Char) (Char.code c)
      | SLitFloat f  -> L.const_float (ltype_of_typ A.Float) f
      | SLitList (l) -> (match t with List(list_typ) -> 
        let len = List.length l in
        let ptr = build_list_malloc (L.const_int (ltype_of_typ A.Int) len) list_typ in
        let rec build_list_stores (el: sexpr list) (curr_offset: int) = 
          match el with
          [] -> []
          | h::t ->  
            let built_expr = build_expr builder h in
            let ptr_offset = L.build_gep byte_t ptr (Array.of_list [(L.const_int byte_t curr_offset)]) "listlitgep" builder in
            ignore(L.build_store built_expr ptr_offset builder); build_list_stores t (curr_offset + (size_of_typ list_typ))
        in
        ignore(build_list_stores l list_start_offset);
        ptr;
        | _ -> raise (Failure ("IR Error (build_expr): SLitList is not list.")))
      | SId (_, cname) -> L.build_load (ltype_of_typ t) (lookup cname) cname builder
      | SBinop (e1, o, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        let e1_t = (match e1 with (e1_typ, _) -> e1_typ) in
        (match o with
           A.Plus    -> if e1_t = A.Float then L.build_fadd else L.build_add
         | A.Minus     -> if e1_t = A.Float then L.build_fsub else L.build_sub
         | A.Times    -> if e1_t = A.Float then L.build_fmul else L.build_mul
         | A.Divide   -> if e1_t = A.Float then L.build_fdiv else L.build_sdiv
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Eq   -> if e1_t = A.Float then  L.build_fcmp L.Fcmp.Oeq else L.build_icmp L.Icmp.Eq
         | A.Neq   -> if e1_t = A.Float then L.build_fcmp L.Fcmp.One else L.build_icmp L.Icmp.Ne 
         | A.Lt    -> if e1_t = A.Float then L.build_fcmp L.Fcmp.Olt else L.build_icmp L.Icmp.Slt
         | A.Leq    -> if e1_t = A.Float then L.build_fcmp L.Fcmp.Ole else L.build_icmp L.Icmp.Sle
         | A.Gt     -> if e1_t = A.Float then L.build_fcmp L.Fcmp.Ogt else L.build_icmp L.Icmp.Sgt
         | A.Geq    -> if e1_t = A.Float then L.build_fcmp L.Fcmp.Oge else L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
      | SUnaryOp (o, e) ->
        let e' = build_expr builder e in 
        (match o with
           A.Negate    ->  L.build_not
        ) e' "tmpu" builder
      | SCall ("printi", arg_list, _) ->
          build_print_call arg_list builder true
      | SCall ("printd", arg_list, _) ->
        build_print_call arg_list builder false
      | SCall ("free", arg_list, _) ->
          let ptr = build_expr builder (List.hd arg_list) in
          ignore(ptr);
          let built_free = L.build_free ptr builder in
          built_free
      | SCall ("len", arg_list, _) ->
          let expr = List.hd arg_list in
          let ptr = build_expr builder expr in
          ignore(ptr);
          let user_size_gep = L.build_gep byte_t ptr (Array.of_list [L.const_int (ltype_of_typ A.Int) 0]) "listlitsizegep" builder in
          let list_size = (L.build_load (ltype_of_typ t) user_size_gep "listindexsizeload" builder) in
          list_size;
      | SCall ("truelen", arg_list, _) ->
          let expr = List.hd arg_list in
          let ptr = build_expr builder expr in
          ignore(ptr);
          let user_size_gep = L.build_gep byte_t ptr (Array.of_list [L.const_int (ltype_of_typ A.Int) 8]) "listlitsizegep" builder in
          let list_size = (L.build_load (ltype_of_typ t) user_size_gep "listindexsizeload" builder) in
          list_size;
      | SCall ("setsizei", arg_list, _) ->
        let expr = List.hd arg_list in
        let ptr = build_expr builder expr in
        let ptr_gep = L.build_gep byte_t ptr (Array.of_list [L.const_int (ltype_of_typ A.Int) 0]) "listlitsizegep" builder in
        ignore(ptr);
        let new_size = build_expr builder (List.hd (List.tl arg_list)) in
        let size_store =  (L.build_store new_size ptr_gep builder) in
        ignore(size_store);
        ptr
      | SCall (_, args, cname) ->
        let (fdef, _) = try StringMap.find cname function_decls with Not_found -> raise (Failure ("IR Error (build_expr): SCall function " ^ cname ^ " not found.")) in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = cname ^ "_result" in
        let arg_types = ltypes_of_typs (types_of_sexprs args) in
        let func_type = L.function_type (ltype_of_typ t) (Array.of_list arg_types) in 
        L.build_call func_type fdef (Array.of_list llargs) result builder
      | SListIndex(_, ind, cname) (*TODO: Check if index requested is outside of "size" *) -> 
        let ptr = (L.build_load (ptr_t) (lookup cname) "listindexptr" builder) in
        ignore(ptr);
        let size_gep = L.build_gep byte_t ptr (Array.of_list [L.const_int (ltype_of_typ A.Int) 0]) "listlitsizegep" builder in
        let list_size = (L.build_load (ltype_of_typ t) size_gep "listindexsizeload" builder) in
        ignore(list_size);
        let built_expr = build_expr builder ind in
        ignore(build_expr);
        let calc_offset = L.build_mul (L.const_int (ltype_of_typ A.Int) (size_of_typ t) ) (built_expr) "listindexmul" builder in
        ignore(calc_offset);
        let add_offset = L.build_add (calc_offset) (L.const_int (ltype_of_typ A.Int) list_start_offset) "listindexadd" builder in
        ignore(add_offset);
        let ptr_gep = L.build_gep byte_t ptr (Array.of_list [add_offset]) "listlitgep" builder in
        let val_load = (L.build_load (ltype_of_typ t) ptr_gep "listindexload" builder) in
        val_load
      | SAssignIndex(_, ind, exp, cname) (*TODO: Check if index requested is outside of "size" *) -> 
        let ptr = (L.build_load (ptr_t) (lookup cname) "listindexptr" builder) in
        ignore(ptr);
        let size_gep = L.build_gep byte_t ptr (Array.of_list [L.const_int (ltype_of_typ A.Int) 0]) "listlitsizegep" builder in
        let list_size = (L.build_load (ltype_of_typ t) size_gep "listindexsizeload" builder) in
        ignore(list_size);
        let built_ind_expr = build_expr builder ind in
        ignore(built_ind_expr);
        let built_expr = build_expr builder exp in 
        ignore(built_expr);
        let calc_offset = L.build_mul (L.const_int (ltype_of_typ A.Int) (size_of_typ t) ) (built_ind_expr) "listindexmul" builder in
        ignore(calc_offset);
        let add_offset = L.build_add (calc_offset) (L.const_int (ltype_of_typ A.Int) list_start_offset) "listindexadd" builder in
        ignore(add_offset);
        let ptr_gep = L.build_gep byte_t ptr (Array.of_list [add_offset]) "listlitgep" builder in
        let val_store = (L.build_store built_expr ptr_gep builder) in
        ignore(val_store);
        built_expr
      | SNewList (list_typ, list_size) (* TODO: Zero-initialize everything *)-> 
        let ptr = build_list_malloc (build_expr builder list_size) list_typ in
        ptr
      | SNull -> L.const_pointer_null ptr_t
    in

    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in
    
    let rec build_stmt builder = function
      | SExprStmt e -> ignore(build_expr builder e); builder
      | SDeclAssign(_, _, e, cname) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup cname) builder); builder
      | SListDeclAssign (_, _, e, cname) -> let e' = build_expr builder e in
      ignore(L.build_store e' (lookup cname) builder); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | SIf (predicate, then_block, else_block) ->
        let then_stmt_list = match then_block with SBlock(then_stmt) -> then_stmt in
        let else_stmt_list = match else_block with SBlock(else_stmt) -> else_stmt in

        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then_block" the_function in
        let then_endbuilder = (List.fold_left build_stmt (L.builder_at_end context then_bb) then_stmt_list) in 
        ignore(then_endbuilder);
        let else_bb = L.append_block context "else_block" the_function in
        let else_endbuilder = (List.fold_left build_stmt (L.builder_at_end context else_bb) else_stmt_list) in 
        ignore(else_endbuilder);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        ignore(L.build_br end_bb then_endbuilder);
        ignore(L.build_br end_bb else_endbuilder);

        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let body_stmt_list = match body with SBlock(sl) -> sl in
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        let body_endbuilder = (List.fold_left build_stmt (L.builder_at_end context body_bb) body_stmt_list) in
        add_terminal (body_endbuilder) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in
        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        
        ignore(L.build_br end_bb body_endbuilder);

        L.builder_at_end context end_bb
      | SListDecl (_, _, _) (*Probably do nothing here, but check *)-> builder
      | SFor (_, _, _) -> builder (* For loops are converted into While loops in the semantics stage *)
      | SForIn (_, _, _, _) -> builder (* For in loops are converted into While loops in the semantics stage *)
      | SDecl(_, _, _) -> builder (* Ignore declarations, which are already covered in 'globals' *)
      | SFdecl(_, _, _, _, _) -> builder (* Ignore function declarations, which are already covered in 'functions'*)
    in
    let sl = match sblock with SBlock(sl) -> sl in
    let func_builder = List.fold_left build_stmt builder sl in 

    (* ignore(if cname = "main" then 
      let build_malloc_frees = 
        let build_malloc_free (lval: L.llvalue) =
          let free_var = L.build_load ptr_t lval "freevar" func_builder in 
          L.build_free free_var func_builder
        in
        StringMap.map build_malloc_free !malloc_list
      in
      ignore(build_malloc_frees)); *)
  
    add_terminal func_builder (L.build_ret ((if rtyp = A.Float then (L.const_float (ltype_of_typ rtyp) 0.0) else L.const_int (ltype_of_typ rtyp) 0)))

    | _ -> raise (Failure("IR Error (build_function_body): Unexpected non-function statement."))
  in

  List.iter build_function_body functions;
  the_module