(* Semantic checking for the LILY compiler *)

open Libparser
open Ast

let preprocess (program_block: program) :program =


  let get_default_return (t:typ): expr =
    match t with
    Int -> LitInt(0)
    | Bool -> LitBool(false)
    | Char -> LitChar('a')
    | Float -> LitFloat(0.0)
    | List(_) -> Null
    | _ -> LitInt 0
  in


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
    (* let rec get_typ_list_helper (remaining_types: typ list): (typ * (typ list)) list  =
      match remaining_types with
      [] -> []
      | hed::tal -> [(hed, replace_any_with_typ args hed)] @ (get_typ_list_helper tal)
    in  *)


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
      | ForIn(id, e, b) -> [ForIn(id, e, (check_block b))]
      | ExprStmt(e) -> [ExprStmt(e)]
      | Return(e) -> [Return(e)]
      | Decl(typ, id) -> [Decl(typ, id)]
      | DeclAssign(et, id, e) ->  [DeclAssign(et, id, e)]
      | Fdecl(t, name, binds, b) -> (check_func (t, name, binds, b))
    in

    (* let fbinds = add_var_binds starting_vars in
    match block with
    Block(sl) -> (fbinds, SBlock(List.map check_stmt sl)) *)

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

  (* let (_, sprogram_block) = check_block program_block FuncMap.empty StringMap.empty [] Void "main" in
  let funcs = SFdecl(Void, "main", [], sprogram_block, "main")::!functions in
  (sprogram_block, !globals, funcs) *)
  check_block program_block