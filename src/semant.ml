(* Semantic checking for the LILY compiler *)

open Libparser
open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (program_block) =

  let check_block (block: block) (b_fmap: typ StringMap.t) (b_vmap: typ StringMap.t): sblock =
    let l_fmap: typ StringMap.t = StringMap.empty 
    in
    let l_vmap: typ StringMap.t = StringMap.empty
    in

    let is_var_local (id: string): bool =
      StringMap.mem id l_vmap
    in
    let is_var (id: string): bool =
      if is_var_local id then true else (StringMap.mem id b_vmap)
    in
    let check_var (id: string) : typ =
      if is_var_local id then (StringMap.find id l_vmap) else (
        if is_var id then (StringMap.find id b_vmap) else  
          raise (Failure ("Undeclared variable " ^ id)))
    in
    let add_var (id: string) (t: typ) =
      if is_var_local id then raise (Failure ("Already declared variable " ^ id ^ " in current scope")) 
      else StringMap.add id t l_vmap
    in

    let rec check_expr (e: expr): sexpr =
      match e with
      LitInt(l) ->  (Int, SLitInt(l))
      | LitBool(l) -> (Bool, SLitBool(l))
      | LitFloat(l) -> (Float, SLitFloat(l))
      | LitChar(l) -> (Char, SLitChar(l))
      | Id(id) -> (check_var id, SId(id))
      (* TODO: Add some Binop support between different types? *)
      | Binop(e1, op, e2) -> (let (t1, se1) = check_expr e1 in let (t2, se2) = check_expr e2 in if t1 != t2 then raise(Failure("variables of different types in binop")) else (t1, SBinop((t1, se1), op, (t2, se2))))
      | Call(s, el) -> SCall(s, el)
      | UnaryOp(op, e) -> SUnaryOp(op, e)
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

  check_block program_block StringMap.empty StringMap.empty