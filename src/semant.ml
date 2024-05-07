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
    (* let l_fmap: typ FuncMap.t = FuncMap.empty 
    in
    let l_vmap: typ StringMap.t = StringMap.empty
    in *)

    let is_var_local (map: typ StringMap.t) (id: string): bool =
      StringMap.mem id map
    in
    let is_var (map: typ StringMap.t) (id: string): bool =
      if is_var_local map id then true else (StringMap.mem id b_vmap)
    in
    let find_var (map: typ StringMap.t) (id: string) : typ =
      ignore(print_endline ("DEBUG: finding var " ^ id ^ "LOCALS: " ^ (map_to_str map) ^ " GLOBALS: " ^ (map_to_str b_vmap)));
      if is_var_local map id then (StringMap.find id map) else (
        if is_var map id then (StringMap.find id b_vmap) else  
          raise (Failure ("Undeclared variable " ^ id)))
    in
    let add_var (map: typ StringMap.t) (id: string) (t: typ):typ StringMap.t =
      ignore(print_endline ("DEBUG: adding var " ^ id ^ "LOCALS: " ^ (map_to_str map) ^ " GLOBALS: " ^ (map_to_str b_vmap)));
      if is_var_local map id then raise (Failure ("Already declared variable " ^ id ^ " in current scope")) 
      else StringMap.add id t map
    in
    let add_var_bind (map: typ StringMap.t) (bind: bind):typ StringMap.t = 
      match bind with
      (t, id) -> add_var map id t
    in
    let add_var_binds (map: typ StringMap.t) (binds: bind list):typ StringMap.t = 
      List.fold_left add_var_bind map binds
    in

    let is_func_local (map: typ FuncMap.t) (name: string) (args: typ list):bool =
      FuncMap.mem {id=name; args=args} map
    in
    let is_func (map: typ FuncMap.t) (name: string) (args: typ list):bool =
      if is_func_local map name args then true else (
        FuncMap.mem {id=name; args=args} b_fmap
      )
    in
    let find_func (map: typ FuncMap.t) (name: string) (args: typ list):typ =
      if is_func_local map name args then (FuncMap.find {id=name; args=args} map) else (
        if is_func map name args then (FuncMap.find {id=name; args=args} b_fmap) else
          raise (Failure ("Function "^ name ^ " with proper args not visible in scope"))
      )
    in
    let add_func (map: typ FuncMap.t) (name: string) (args: typ list) (t: typ):typ FuncMap.t =
      if is_func_local map name args then (raise (Failure ("Already declared variable " ^ name ^ " in current scope"))) else
      FuncMap.add {id=name; args=args} t map
    in
    
    let rec check_expr_list(vmap: typ StringMap.t) (fmap: typ FuncMap.t) (el: expr list): sexpr list =
      let partial_application = check_expr vmap fmap in
      List.map partial_application el
    and check_expr (vmap: typ StringMap.t) (fmap: typ FuncMap.t) (e: expr): sexpr =
      match e with
      LitInt(l) ->  (Int, SLitInt(l))
      | LitBool(l) -> (Bool, SLitBool(l))
      | LitFloat(l) -> (Float, SLitFloat(l))
      | LitChar(l) -> (Char, SLitChar(l))
      | Id(id) -> (find_var vmap id, SId(id))
      (* TODO: Add some Binop support between different types? *)
      | Binop(e1, op, e2) -> (let (t1, se1) = check_expr vmap fmap e1 in let (t2, se2) = check_expr vmap fmap e2 in if t1 != t2 then raise(Failure("variables of different types in binop")) else (t1, SBinop((t1, se1), op, (t2, se2))))
      (* TODO more call checks *)
      | Call(name, el) -> 
        let sel =  check_expr_list vmap fmap el in
        let args = List.map sexpr_to_typ sel in
        (find_func fmap name args, 
        SCall(name, sel))
      (* TODO more unary op checks based on operators *)
      | UnaryOp(op, e) -> let (t, se) = check_expr vmap fmap e in (t, SUnaryOp(op, (t, se)))
    in

    let check_binds (binds : bind list) =
      let rec dups = function
          [] -> ()
        |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
          raise (Failure ("duplicate bind " ^ n1))
        | _ :: t -> dups t
      in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
    in
    let check_func (vmap: typ StringMap.t) (fmap: typ FuncMap.t) (t: typ) (name: string) (binds: bind list) (b: block): sstmt =
      ignore(check_binds binds); 
      let args = bind_list_to_typ_list binds in
      (* ignore(add_func name args t); *)
      let fmap_new = add_func fmap name args t in 
      let sb = check_block b (FuncMap.union pick_fst fmap_new b_fmap) (StringMap.union pick_fst vmap b_vmap) binds t in
      SFdecl(t, name, binds, sb)
    in
    let rec check_stmt (vmap: typ StringMap.t) (fmap: typ FuncMap.t) (s: stmt): sstmt =
      match s with 
      (* TODO: make assignment an expression? Implicit assignment seems easy here? *)
      Assign(var, e) -> let (t, se) = check_expr vmap fmap e in let et = find_var vmap var in if t == et then SAssign(var, (t, se)) else raise (Failure "Assigning variable that wasn't declared.")
      | If (e, b1, b2) -> let (t, se) = check_expr vmap fmap e in ignore(if t != Bool then raise (Failure ("If statement expression not boolean")));
        let sb1 = check_block b1 (FuncMap.union pick_fst fmap b_fmap) (StringMap.union pick_fst vmap b_vmap) [] Void in 
        let sb2 = check_block b2 (FuncMap.union pick_fst fmap b_fmap) (StringMap.union pick_fst vmap b_vmap) [] Void in 
        SIf((t, se), sb1, sb2)
      | While(e, b) -> let (t, se) = check_expr vmap fmap e in ignore(if t != Bool then raise (Failure ("If statemen rec t expression not boolean")));
        let sb = check_block b (FuncMap.union pick_fst fmap b_fmap) (StringMap.union pick_fst vmap b_vmap) [] Void in 
        SWhile((t, se), sb)
      | For(e, s, b) -> let (t, se) = check_expr vmap fmap e in ignore(if t != Bool then raise (Failure ("If statement expression not boolean")));
        let sb = check_block b (FuncMap.union pick_fst fmap b_fmap) (StringMap.union pick_fst vmap b_vmap) [] Void in 
        SFor((t, se), check_stmt vmap fmap s , sb)
      | ExprStmt(e) -> SExprStmt(check_expr vmap fmap e)
      | Return(e) -> let (t, se) = check_expr vmap fmap e in if t != block_return then raise (Failure ("Returned invalid type")) else SReturn(t, se)
      | Decl(typ, id) -> ignore(add_var id typ); SDecl(typ, id)
      | DeclAssign(et, id, e) ->  ignore(add_var id et); let (t, se) = check_expr vmap fmap e in if t == et then SDeclAssign(et, id, (t, se)) else raise (Failure "Assigning variable that wasn't declared.")
      | Fdecl(t, name, binds, b) -> check_func vmap fmap t name binds b
      (* | _ -> SReturn((Bool, SLitBool(true))) *)
    in

    (* ignore(add_var_binds starting_vars); *)
    (* ignore(print_endline ("DEBUG: STARTING BLOCK " ^ "LOCALS: " ^ (map_to_str) ^ " GLOBALS: " ^ (map_to_str b_vmap))); *)
    let var_binds = add_var_binds StringMap.empty starting_vars in
    let partial_application = check_stmt (var_binds) FuncMap.empty in
    match block with
    Block(sl) -> SBlock(List.map partial_application sl)
  in

  check_block program_block FuncMap.empty StringMap.empty [] Void