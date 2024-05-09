(* Semantic checking for the LILY compiler *)

open Libparser
open Ast
open Sast
open Structs

module StringMap = Map.Make(String)
module FuncMap = Map.Make(FuncId)

let check (program_block) =
  let reserved_funcs: (typ * string * ((typ list) option) * int) list =[
    (* rtyp, name, args (None for any), min_args*)
  (Any,"printi", None, 1)
  ] in
  let reserved_func_names: string list = [
    "printi"
  ] in

  (* let print_count = ref 0 in *)
  let functions = ref [] in 
  let globals = ref [] in
  let vnames: int StringMap.t ref = ref StringMap.empty in
  let fnames: int StringMap.t ref = ref StringMap.empty in 
  let update_vnames (id: string):int = 
    if StringMap.mem id !vnames then (
      let curr_val = StringMap.find id !vnames in 
        ignore(vnames := StringMap.add id (curr_val + 1) !vnames); (curr_val + 1))
    else (ignore(vnames := StringMap.add id 0 !vnames); 0)
  in
  let update_fnames (id: string):int = 
    if StringMap.mem id !fnames then (
      let curr_val = StringMap.find id !fnames in 
        ignore(fnames := StringMap.add id (curr_val + 1) !fnames); (curr_val + 1))
    else (ignore(fnames := StringMap.add id 0 !fnames); 0)
  in

  let bind_to_typ (bind: bind): typ =
    match bind with (t, _) -> t
  in
  let bind_list_to_typ_list (bl: bind list): typ list =
    List.map bind_to_typ bl
  in
  let sexpr_to_typ (se: sexpr): typ =
    match se with
      (t, _) -> t
  in
  let pick_fst _ v1 _ = Some v1 in


  let rec check_block (block: block) (b_fmap: bind FuncMap.t) (b_vmap: bind StringMap.t) (starting_vars: bind list) (block_return: typ) (block_name: string): sbind list * sblock =
    let l_fmap: bind FuncMap.t ref = ref FuncMap.empty in
    let l_vmap: bind StringMap.t ref = ref StringMap.empty in

    (* TODO: Check everything in list is same type *)

    let is_var_local (id: string): bool =
      StringMap.mem id !l_vmap
    in
    let is_var (id: string): bool =
      if is_var_local id then true else (StringMap.mem id b_vmap)
    in
    let find_var (id: string) : bind =
      if is_var_local id then (StringMap.find id !l_vmap) else (
        if is_var id then (StringMap.find id b_vmap) else  
          raise (Failure ("Semantics Error (find_var): Undeclared variable " ^ id)))
    in
    let add_var (id: string) (t: typ) (global: bool): string =
      if is_var_local id then raise (Failure ("Semantics Error (add_var): Already declared variable " ^ id ^ " in current scope")) 
      else (
        if not global then (
          ignore(l_vmap := StringMap.add id (t, id) !l_vmap);
          id
        ) else (
        let vname_number = update_vnames id in 
        let cname = (id ^ "!" ^ (string_of_int vname_number)) in
        ignore(l_vmap := StringMap.add id (t, cname) !l_vmap);
        ignore(globals := (t, id, cname)::!globals);
        cname))
    in
    let add_var_bind (bind: bind): sbind = 
      match bind with
      (t, id) -> 
        let cname = add_var id t true in
        (t, id, cname)
    in
    let add_var_binds (binds: bind list): sbind list = 
      List.map add_var_bind binds
    in

    let is_func_reserved (name: string) (args: typ list): bind option =
      let reserved_has_name (name:string): (typ * string * ((typ list) option) * int) option = 
        let rec res_check (l) (name: string): (typ * string * ((typ list) option) * int) option =
          match l with
          [] -> None
          | h::t -> 
            match h with (_, hname, _, _) -> if hname = name then Some(h) else res_check t name
        in
        res_check reserved_funcs name
      in
      match reserved_has_name name with None ->  None
      | Some(t, n, xargs, nargs) -> match xargs with None -> (if List.length args >= nargs then Some(t, n) else None)
      | Some(rargs) -> if rargs = args then Some(t, n) else None
    in
    let is_func_local (name: string) (args: typ list):bool =
      FuncMap.mem {id=name; args=args} !l_fmap
    in
    let is_func (name: string) (args: typ list):bool =
      if is_func_local name args then true else (
        FuncMap.mem {id=name; args=args} b_fmap
      )
    in
    let find_func (name: string) (args: typ list):bind =
      match is_func_reserved name args with Some(x, y) -> (x, y) | None -> (
      if is_func_local name args then (FuncMap.find {id=name; args=args} !l_fmap) else (
        if is_func name args then (FuncMap.find {id=name; args=args} b_fmap) else
          raise (Failure ("Semantics Error (find_func): Function "^ name ^ " with proper args not visible in scope")))
      )
    in
    let add_func (name: string) (args: typ list) (t: typ): string =
      if List.mem name reserved_func_names then (raise (Failure ("Semantics Error (add_func): Cannot name a function " ^ name ^ " (reserved)"))) else
      if is_func_local name args then (raise (Failure ("Semantics Error (add_func): Already declared variable " ^ name ^ " in current scope")))
      else (
        let func_number = update_fnames name in 
        let cname = (name ^ "!" ^ (string_of_int func_number)) in
        ignore(l_fmap := FuncMap.add {id=name; args=args} (t, cname) !l_fmap);
        cname)
    in

    let is_boolean_op (op: op): bool =
      match op with 
      Eq -> true
      | Neq -> true
      | Lt -> true
      | Leq -> true
      | Gt -> true
      | Geq -> true
      | And -> true
      | Or -> true
      | _ -> false
    in
    let rec check_expr (e: expr): sexpr =
      match e with
      Assign(var, e) -> let (t, se) = check_expr e in let (et, cname) = find_var var in if t = Any || t = et then (t, SAssign(var, (t, se), cname)) else raise (Failure ("Semantics Error (check_stmt): Assigning variable " ^ var ^ "(type " ^ string_of_typ et ^ ", expression " ^ string_of_typ t ^ ") that wasn't declared in block " ^ block_name))
      | LitInt(l) ->  (Int, SLitInt(l))
      | LitBool(l) -> (Bool, SLitBool(l))
      | LitFloat(l) -> (Float, SLitFloat(l))
      | LitChar(l) -> (Char, SLitChar(l))
      | LitList(_) (* TODO *)-> (Int, SLitInt(1))
      | Id(id) -> let (t, cname) = find_var id in (t, SId(id, cname))
      (* TODO: Add some Binop support between different types? *)
      | Binop(e1, op, e2) -> (let (t1, se1) = check_expr e1 in let (t2, se2) = check_expr e2 in 
      (if t1 != t2 then raise(Failure("Semantics Error (check_expr): Variables of different types in Binop")) 
      else (
        let (_:op) = (
        match op with 
          And -> (if t1 != Bool then raise(Failure ("Semantics Error (check_expr): Non-boolean expr in And operation")) else op)
        | Or -> (if t1 != Bool then raise(Failure ("Semantics Error (check_expr): Non-boolean expr in Or operation")) else op)
        | _ -> op)
        in (let op_typ = if is_boolean_op op then Bool else t1 in
          op_typ, SBinop((t1, se1), op, (t2, se2))))))
      | Call("print", el) ->
        let sel = List.map check_expr el in
        let args = List.map sexpr_to_typ sel in
        if is_func "print" args then
        (
          let (t, cname) = find_func "print" args in
          (t, SCall("print", sel, cname))
        ) else (
          let ret_typ = List.hd args in
          let bind_count = ref 0 in
          let typ_to_bind (t : typ) : bind = 
            ignore(bind_count := !bind_count + 1);
            let var_name = "arg" ^ (string_of_int !bind_count) in
            (t, var_name)
          in
          let print_binds = match args with _::t -> [(ret_typ,"arg_return")] @ (List.map typ_to_bind t) | [] -> [] in
          let bind_to_arg (b: bind): expr = 
            match b with (_, aname) -> Id(aname)
          in
          let call_names = List.map bind_to_arg print_binds in
          let print_body = Block([DeclAssign(ret_typ, "print_return", (Call("printi", call_names))); (Return((  Id("arg_return"))))])  in
          let print_decl = Fdecl(ret_typ, "print", print_binds , print_body) in
          ignore(check_stmt print_decl);
          let (t, cname) = find_func "print" args in
          (t, SCall("print", sel, cname))
        )
      | Call(name, el) -> 
        let sel = List.map check_expr el in
        let args = List.map sexpr_to_typ sel in
        let (t, cname) = find_func name args in
        (t, SCall(name, sel, cname))
      | UnaryOp(op, e) -> let (t, se) = check_expr e in (
        match op with
        Negate -> if t = Bool then (t, SUnaryOp(op, (t, se))) else raise (Failure ("Semantics Error (check_expr): Non-Boolean Unary Operator Call in Block " ^ block_name)))
      | ListIndex(_, _) (*TODO*) -> (Int, SLitInt(1))

    and check_binds (binds : (typ * string) list) =
      let rec dups = function
          [] -> ()
        |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
          raise (Failure ("Semantics Error (check_binds): Duplicate Bind " ^ n1))
        | _ :: t -> dups t
      in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
    (* TODO CHECK THAT FUNCTION HAS A RETURN STATEMENT IF IT RETURNS NON-VOID *)
    and check_func (t: typ) (name: string) (binds: bind list) (b: block): sstmt =
      ignore(check_binds binds); 
      let args = bind_list_to_typ_list binds in
      let cname = add_func name args t in
      let (sbinds, sb) = check_block b (FuncMap.union pick_fst !l_fmap b_fmap) (StringMap.union pick_fst !l_vmap b_vmap) binds t name in
      let sfdecl = SFdecl(t, name, sbinds, sb, cname) in
      ignore(functions := sfdecl::!functions);
      sfdecl
    and check_stmt (s: stmt): sstmt =
      match s with 
      (* Assign(var, e) -> let (t, se) = check_expr e in let (et, cname) = find_var var in if t = et then SAssign(var, (t, se), cname) else raise (Failure ("Semantics Error (check_stmt): Assigning variable " ^ var ^ " that wasn't declared in block " ^ block_name)) *)
      | If (e, b1, b2) -> let (t, se) = check_expr e in ignore(if t != Bool then raise (Failure ("Semantics Error (check_stmt): If statement expression not boolean in Block " ^ block_name)));
        let (_, sb1) = check_block b1 (FuncMap.union pick_fst !l_fmap b_fmap) (StringMap.union pick_fst !l_vmap b_vmap) [] block_return block_name in 
        let (_, sb2) = check_block b2 (FuncMap.union pick_fst !l_fmap b_fmap) (StringMap.union pick_fst !l_vmap b_vmap) [] block_return block_name in 
        SIf((t, se), sb1, sb2)
      | While(e, b) -> let (t, se) = check_expr e in ignore(if t != Bool then raise (Failure ("Semantics Error (check_stmt): While statement expression not boolean in Block " ^ block_name)));
        let (_, sb) = check_block b (FuncMap.union pick_fst !l_fmap b_fmap) (StringMap.union pick_fst !l_vmap b_vmap) [] block_return block_name in 
        SWhile((t, se), sb)
      | For(e, a, b) -> let (t, se) = check_expr e in ignore(if t != Bool then raise (Failure ("Semantics Error (check_stmt): For loop statement expression not boolean in Block " ^ block_name)));
        let (_, sb) = check_block b (FuncMap.union pick_fst !l_fmap b_fmap) (StringMap.union pick_fst !l_vmap b_vmap) [] block_return block_name in 
        let sl = match sb with SBlock(sl) -> sl in
        let sl_new = (SExprStmt(check_expr a))::sl in
        SWhile((t, se), SBlock(sl_new))
      | ExprStmt(e) -> SExprStmt(check_expr e)
      | Return(e) -> let (t, se) = check_expr e in if t != block_return then raise (Failure ("Semantics Error (check_stmt): Returned invalid type " ^ (string_of_typ t) ^ "in Block " ^ block_name)) else SReturn(t, se)
      | Decl(typ, id) (*TODO handle list declaration*) -> let cname = add_var id typ true in SDecl(typ, id, cname)
      | DeclAssign(et, id, e) (*TODO handle list declaration*) ->  let cname = add_var id et true in let (t, se) = check_expr e in if t = Any || t = et then SDeclAssign(et, id, (t, se), cname) else raise (Failure ("Semantics Error (check_stmt): DeclAssigning variable " ^ id ^ " to var of wrong type " ^ string_of_typ t ^ " (expected " ^ string_of_typ et ^ ")" ^ " in Block " ^ block_name))
      | Fdecl(t, name, binds, b) -> check_func t name binds b
    in

    let fbinds = add_var_binds starting_vars in
    match block with
    Block(sl) -> (fbinds, SBlock(List.map check_stmt sl))
  in

  let (_, sprogram_block) = check_block program_block FuncMap.empty StringMap.empty [] Void "main" in
  let funcs = SFdecl(Void, "main", [], sprogram_block, "main")::!functions in
  (sprogram_block, !globals, funcs)