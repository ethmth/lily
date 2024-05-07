(* Semantic checking for the LILY compiler *)

open Libparser
open Ast
open Sast
open Modules

module StringMap = Map.Make(String)
module FuncMap = Map.Make(FuncId)

(* type point = { x : int; y : int }

module PointCompare = struct
  type t = point
  let compare p1 p2 =
    match compare p1.x p2.x with
    | 0 -> compare p1.y p2.y  (* If x coordinates are equal, compare y coordinates *)
    | c -> c
end

module PointMap = Map.Make(PointCompare) *)




let check (program_block) =
  (* let bind_to_typ (bind: bind): typ =
    match bind with (t, _) -> t
  in
  let bind_list_to_typ_list (bl: bind list): typ list =
    List.map bind_to_typ bl
  in *)

  (* let map = PointMap.empty  (* Creates an empty map *) in
let map = PointMap.add {x=1; y=2} "Point at (1,2)" map in
let map = PointMap.add {x=3; y=4} "Point at (3,4)" map in *)

  let funcMapAdd (t: FuncMap.key) (map: typ FuncMap.t) =
    FuncMap.add t Int map
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
    let check_var (id: string) : typ =
      if is_var_local id then (StringMap.find id l_vmap) else (
        if is_var id then (StringMap.find id b_vmap) else  
          raise (Failure ("Undeclared variable " ^ id)))
    in
    let add_var (id: string) (t: typ) =
      if is_var_local id then raise (Failure ("Already declared variable " ^ id ^ " in current scope")) 
      else StringMap.add id t l_vmap
    in

    let is_func_local (id: string) (args: typ list) =
      (* let t = (id, args) in *)
      funcMapAdd {id; args} l_fmap
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

  check_block program_block FuncMap.empty StringMap.empty