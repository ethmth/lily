(* Semantic checking for the LILY compiler *)

open Libparser
open Ast
open Sast
open Modules

module StringMap = Map.Make(String)
module FuncMap = Map.Make(FuncId)

let check (program_block) =
  (* let bind_to_typ (bind: bind): typ =
    match bind with (t, _) -> t
  in
  let bind_list_to_typ_list (bl: bind list): typ list =
    List.map bind_to_typ bl
  in *)
  let sexpr_to_typ (se: sexpr): typ =
    match se with
      (t, _) -> t
  in


  let check_block (block: block) (b_fmap: typ FuncMap.t) (b_vmap: typ StringMap.t): sblock =
    let l_fmap: typ FuncMap.t = FuncMap.empty 
    in
    let l_vmap: typ StringMap.t = StringMap.empty
    in

    let is_var_local (id: string): bool =
      StringMap.mem id l_vmap
    in
    let is_var (id: string): bool =
      if is_var_local id then true else (StringMap.mem id b_vmap)
    in
    let find_var (id: string) : typ =
      if is_var_local id then (StringMap.find id l_vmap) else (
        if is_var id then (StringMap.find id b_vmap) else  
          raise (Failure ("Undeclared variable " ^ id)))
    in
    let add_var (id: string) (t: typ) =
      if is_var_local id then raise (Failure ("Already declared variable " ^ id ^ " in current scope")) 
      else StringMap.add id t l_vmap
    in

    let is_func_local (name: string) (args: typ list):bool =
      FuncMap.mem {id=name; args=args} l_fmap
    in
    let is_func (name: string) (args: typ list):bool =
      if is_func_local name args then true else (
        FuncMap.mem {id=name; args=args} b_fmap
      )
    in
    let find_func (name: string) (args: typ list):typ =
      if is_func_local name args then (FuncMap.find {id=name; args=args} l_fmap) else (
        if is_func name args then (FuncMap.find {id=name; args=args} b_fmap) else
          raise (Failure ("Function "^ name ^ " with proper args not visible in scope"))
      )
    in
    let add_func (name: string) (args: typ list) (t: typ) =
      FuncMap.add {id=name; args=args} t l_fmap
    in

    let rec check_expr (e: expr): sexpr =
      match e with
      LitInt(l) ->  (Int, SLitInt(l))
      | LitBool(l) -> (Bool, SLitBool(l))
      | LitFloat(l) -> (Float, SLitFloat(l))
      | LitChar(l) -> (Char, SLitChar(l))
      | Id(id) -> (find_var id, SId(id))
      (* TODO: Add some Binop support between different types? *)
      | Binop(e1, op, e2) -> (let (t1, se1) = check_expr e1 in let (t2, se2) = check_expr e2 in if t1 != t2 then raise(Failure("variables of different types in binop")) else (t1, SBinop((t1, se1), op, (t2, se2))))
      | Call(name, el) -> 
        let sel = List.map check_expr el in
        let args = List.map sexpr_to_typ sel in
        (find_func name args, 
        SCall(name, sel))
        (* let name = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args')) *)
      | UnaryOp(op, e) -> let (t, se) = check_expr e in (t, SUnaryOp(op, (t, se)))
    in

    let check_stmt (s: stmt): sstmt =
      (* ignore(s);
      SReturn(Int, SLitInt(3)) *)
      match s with 
      (* Assign(var, e) -> ignore(add_var var (check_expr_typ e)); SAssign(var, check_expr e) *)
      | _ -> SReturn(Int, SLitInt(3))
    in

    match block with
    Block(sl) -> SBlock(List.map check_stmt sl)
  in

  check_block program_block FuncMap.empty StringMap.empty