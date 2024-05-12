(* Semantic checking for the LILY compiler *)

open Libparser
open Ast
(* open Sast *)

let preprocess (program_block: program) :program =


  let get_default_return (t:typ): expr =
    match t with
    Int -> LitInt(0)
    | Bool -> LitBool(false)
    | Char -> LitChar('a')
    | Float -> LitFloat(0.0)
    | List(_) -> Null
    | _ -> LitInt(0)
  in

  (* let rec check_list (el: expr list): sexpr =
    let rec check_list_helper (el: expr list) (etyp: typ): sexpr list =
      match el with 
      [] -> ([])
      | h::tail -> 
        (let (t, e) = check_expr h in
          if (etyp != Any && t != etyp) then (raise (Failure "Semantics Error (check_list): Not all list members are of the same type.")) else
          ([(t, e)] @ (check_list_helper tail t))) in
    match el with 
    [] -> (List(Int), SLitList([]))
    | h::t -> 
      (
        let (ht, he) = check_expr h in
        (List(ht), SLitList( [(ht, he)] @ check_list_helper t ht))
      ) *)


  let rec check_block (block: block): block =

    let bind_to_typ (bind: bind): typ =
      match bind with (t, _) -> t
    in
    let bind_to_name (bind: bind): string =
      match bind with (_, n) -> n
    in
    let bind_list_to_typ_list (bl: bind list): typ list =
      List.map bind_to_typ bl
    in
    let bind_list_to_names (bl: bind list): string list =
      List.map bind_to_name bl
    in
    let form_bind (t: typ) (n: string) =
      (t, n)
    in
    let form_binds (tl: typ list) (nl: string list) =
      List.map2 form_bind tl nl
    in


    let has_any (args: typ list) =
      List.mem (List(Any)) args
    in

    let replace_any_with_typ (arg_list: typ list) (new_t: typ): typ list =
      List.map (fun x -> (if x = List(Any) then (List(new_t)) else x)) arg_list
    in

    let check_func (t, name, binds, b): stmt list =
      let ret_stmt = Return(get_default_return t )  in
      let ret_block = match b with Block(sl) -> Block(sl @ [ret_stmt]) in

      let args = bind_list_to_typ_list binds in
      let bind_names = bind_list_to_names binds in


      let make_fdecl_for_t (lt: typ): stmt =
        let new_types = replace_any_with_typ args lt in
        let new_binds = form_binds new_types bind_names in
        let ret_typ = (if t = List(Any) then List(lt) else t) in
        Fdecl(ret_typ, name, new_binds, ret_block)
      in

      if has_any args then 
        List.map make_fdecl_for_t [Int; Bool; Char; Float]
      else
        [Fdecl(t, name ,binds, ret_block)]
    in
   
    let check_stmt (s: stmt): stmt list =
      match s with 
      If (e, b1, b2) -> [If(e, (check_block b1), check_block b2)]
      | While(e, b) -> [While(e, (check_block b))]
      | For(e, a, b) -> [For(e, a, (check_block b))]
      | ForIn(id, e, b) -> [ForIn(id, e, b)]
          (* let varname = "forvar!!f" in
          let listname = "forlist!!l" in
          let index_var_stmt = DeclAssign(Int, varname, LitInt(0)) in
          let list_sexpr = DeclAssign(List, listname, e) in
          let (_, id_decl) = (list_typ, SDecl(list_typ, id, id_cname)) in

          (* Loop *)
          let while_condition = (Bool, SBinop((Int, SId(varname, varname_cname)), Lt, (Int, SCall("len", [(t, se)], "len")))) in
          
          (* In Loop *)
          let id_assign = (list_typ, SAssign(id, (list_typ, SListIndex(listname, (Int, SId(varname, varname_cname)), listname_cname)), id_cname)) in
          let index_incrememt = (Int, SAssign(varname, (Int, SBinop( (Int, SId(varname, varname_cname)), Plus, (Int, SLitInt(1)))), varname_cname)) in
          
          (* Combine it *)
          let (_, checked_block) = check_block b (FuncMap.union pick_fst !l_fmap b_fmap) (StringMap.union pick_fst !l_vmap b_vmap) [] block_return block_name in 
          let while_block = (match checked_block with SBlock(sl) -> ([SExprStmt(id_assign)] @ sl @ [SExprStmt(index_incrememt)])) in
          let while_loop = SWhile(while_condition, SBlock(while_block)) in
          let blck = [index_var_stmt] @ [SExprStmt(list_sexpr)] @ [id_decl] @ [while_loop] in 
          let combined = (SIf((Bool, SLitBool(true)) ,SBlock(blck) , SBlock([]))) in 
          combined
          | _ -> raise (Failure (""))) [ForIn(id, e, (check_block b))] *)
      | ExprStmt(e) -> [ExprStmt(e)]
      | Return(e) -> [Return(e)]
      | Decl(typ, id) -> [Decl(typ, id)]
      | DeclAssign(et, id, e) ->  [DeclAssign(et, id, e)]
      | Fdecl(t, name, binds, b) -> (check_func (t, name, binds, b))
    in

    let reform_block (sll: stmt list list) : stmt list =
      let rec get_sl (sll: stmt list list) = 
        match sll with 
        [] -> []
        | h ::t -> h @ (get_sl t)
      in
      get_sl sll
    in
    match block with Block(sl) ->
    let sll = List.map check_stmt (sl) in
    Block(reform_block sll)
  in

  check_block program_block