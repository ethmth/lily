(* Semantic checking for the LILY compiler *)

open Libparser
open Ast

let expand_fdecls (program_block: program) :program =


  let rec check_block (block: block): block =
   
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
      | Fdecl(t, name, binds, b) -> [Fdecl(t, name, binds, check_block b)]
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
    let sll = List.map check_stmt sl in
    Block(reform_block sll)
  in

  (* let (_, sprogram_block) = check_block program_block FuncMap.empty StringMap.empty [] Void "main" in
  let funcs = SFdecl(Void, "main", [], sprogram_block, "main")::!functions in
  (sprogram_block, !globals, funcs) *)
  check_block program_block