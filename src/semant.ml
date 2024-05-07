(* Semantic checking for the LILY compiler *)

open Libparser
open Ast
open Sast
open Modules

module StringMap = Map.Make(String)
module FuncMap = Map.Make(FuncId)

let map_to_str m = 
  let inners = List.map (fun (k, v) -> k ^ " -> " ^ (string_of_typ v)) (StringMap.bindings m)
  in "[" ^ (String.concat ", " inners) ^ "]"


let check (program_block) =
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


  let rec check_block (block: block) (b_fmap: typ FuncMap.t) (b_vmap: typ StringMap.t) (starting_vars: bind list) (block_return: typ): sblock =
    let l_fmap: typ FuncMap.t ref = ref FuncMap.empty 
    in
    let l_vmap: typ StringMap.t ref = ref StringMap.empty
    in

    let is_var_local (id: string): bool =
      StringMap.mem id !l_vmap
    in
    let is_var (id: string): bool =
      if is_var_local id then true else (StringMap.mem id b_vmap)
    in
    let find_var (id: string) : typ =
      ignore(print_endline ("DEBUG: finding var " ^ id ^ "LOCALS: " ^ (map_to_str !l_vmap) ^ " GLOBALS: " ^ (map_to_str b_vmap)));
      if is_var_local id then (StringMap.find id !l_vmap) else (
        if is_var id then (StringMap.find id b_vmap) else  
          raise (Failure ("Undeclared variable " ^ id)))
    in
    let add_var (id: string) (t: typ) =
      ignore(print_endline ("DEBUG: adding var " ^ id ^ "LOCALS: " ^ (map_to_str !l_vmap) ^ " GLOBALS: " ^ (map_to_str b_vmap)));
      if is_var_local id then raise (Failure ("Already declared variable " ^ id ^ " in current scope")) 
      else l_vmap := StringMap.add id t !l_vmap
    in
    let add_var_bind (bind: bind) = 
      match bind with
      (t, id) -> add_var id t
    in
    let add_var_binds (binds: bind list) = 
      List.map add_var_bind binds
    in

    let is_func_local (name: string) (args: typ list):bool =
      FuncMap.mem {id=name; args=args} !l_fmap
    in
    let is_func (name: string) (args: typ list):bool =
      if is_func_local name args then true else (
        FuncMap.mem {id=name; args=args} b_fmap
      )
    in
    let find_func (name: string) (args: typ list):typ =
      if is_func_local name args then (FuncMap.find {id=name; args=args} !l_fmap) else (
        if is_func name args then (FuncMap.find {id=name; args=args} b_fmap) else
          raise (Failure ("Function "^ name ^ " with proper args not visible in scope"))
      )
    in
    let add_func (name: string) (args: typ list) (t: typ) =
      if is_func_local name args then (raise (Failure ("Already declared variable " ^ name ^ " in current scope"))) else
      l_fmap := FuncMap.add {id=name; args=args} t !l_fmap
    in

    let is_boolean_op (op: op): bool =
      match op with 
      Eq -> true
      | Neq -> true
      | Lt -> true
      | Leq -> true
      | Gt -> true
      | Geq -> true
      | _ -> false
    in
    let rec check_expr (e: expr): sexpr =
      match e with
      LitInt(l) ->  (Int, SLitInt(l))
      | LitBool(l) -> (Bool, SLitBool(l))
      | LitFloat(l) -> (Float, SLitFloat(l))
      | LitChar(l) -> (Char, SLitChar(l))
      | Id(id) -> (find_var id, SId(id))
      (* TODO: Add some Binop support between different types? *)
      | Binop(e1, op, e2) -> (let (t1, se1) = check_expr e1 in let (t2, se2) = check_expr e2 in 
        if t1 != t2 then raise(Failure("variables of different types in binop")) 
        else (
          let op_typ = if is_boolean_op op then Bool else t1 in
          op_typ, SBinop((t1, se1), op, (t2, se2))))
      (* TODO more call checks *)
      | Call(name, el) -> 
        let sel = List.map check_expr el in
        let args = List.map sexpr_to_typ sel in
        (find_func name args, 
        SCall(name, sel))
      (* TODO more unary op checks based on operators *)
      | UnaryOp(op, e) -> let (t, se) = check_expr e in (t, SUnaryOp(op, (t, se)))
    in

    let check_binds (binds : (typ * string) list) =
      let rec dups = function
          [] -> ()
        |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
          raise (Failure ("duplicate bind " ^ n1))
        | _ :: t -> dups t
      in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
    in
    let check_func (t: typ) (name: string) (binds: bind list) (b: block): sstmt =
      ignore(check_binds binds); 
      let args = bind_list_to_typ_list binds in
      ignore(add_func name args t);
      let sb = check_block b (FuncMap.union pick_fst !l_fmap b_fmap) (StringMap.union pick_fst !l_vmap b_vmap) binds t in
      SFdecl(t, name, binds, sb)
    in
    let rec check_stmt (s: stmt): sstmt =
      match s with 
      (* TODO: make assignment an expression? Implicit assignment seems easy here? *)
      Assign(var, e) -> let (t, se) = check_expr e in let et = find_var var in if t == et then SAssign(var, (t, se)) else raise (Failure "Assigning variable that wasn't declared.")
      | If (e, b1, b2) -> let (t, se) = check_expr e in ignore(if t != Bool then raise (Failure ("If statement expression not boolean")));
        let sb1 = check_block b1 (FuncMap.union pick_fst !l_fmap b_fmap) (StringMap.union pick_fst !l_vmap b_vmap) [] Void in 
        let sb2 = check_block b2 (FuncMap.union pick_fst !l_fmap b_fmap) (StringMap.union pick_fst !l_vmap b_vmap) [] Void in 
        SIf((t, se), sb1, sb2)
      | While(e, b) -> let (t, se) = check_expr e in ignore(if t != Bool then raise (Failure ("If statemen rec t expression not boolean")));
        let sb = check_block b (FuncMap.union pick_fst !l_fmap b_fmap) (StringMap.union pick_fst !l_vmap b_vmap) [] Void in 
        SWhile((t, se), sb)
      | For(e, s, b) -> let (t, se) = check_expr e in ignore(if t != Bool then raise (Failure ("If statement expression not boolean")));
        let sb = check_block b (FuncMap.union pick_fst !l_fmap b_fmap) (StringMap.union pick_fst !l_vmap b_vmap) [] Void in 
        SFor((t, se), check_stmt s , sb)
      | ExprStmt(e) -> SExprStmt(check_expr e)
      | Return(e) -> let (t, se) = check_expr e in if t != block_return then raise (Failure ("Returned invalid type")) else SReturn(t, se)
      | Decl(typ, id) -> ignore(add_var id typ); SDecl(typ, id)
      | DeclAssign(et, id, e) ->  ignore(add_var id et); let (t, se) = check_expr e in if t == et then SDeclAssign(et, id, (t, se)) else raise (Failure "Assigning variable that wasn't declared.")
      | Fdecl(t, name, binds, b) -> check_func t name binds b
      (* | _ -> SReturn((Bool, SLitBool(true))) *)
    in

    ignore(add_var_binds starting_vars);
    ignore(print_endline ("DEBUG: STARTING BLOCK " ^ "LOCALS: " ^ (map_to_str !l_vmap) ^ " GLOBALS: " ^ (map_to_str b_vmap)));
    match block with
    Block(sl) -> SBlock(List.map check_stmt sl)
  in

  check_block program_block FuncMap.empty StringMap.empty [] Void