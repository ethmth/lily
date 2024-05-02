(* Author: Michaela Gary *)
(* Last Edited: May 1, 2024 *)
(* Semantically-checked Abstract Syntax Tree and functions for printing it in Lily *)

open Ast

type sexpr = typ * expr_detail
and expr_detail =
    SLitInt of int
  | SLitBool of bool
  | SLitFloat of float
  | SLitChar of char
  | SLitString of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SCall of string * sexpr list
  | SListExpr of sexpr list
  | SUnaryOp of unary_op * sexpr
  | SListLit of sexpr list 
  | SListInit of string * typ * sexpr list
  (* | SDeclExpr of styp * string * sexpr *)
  | SListBinop of sexpr * list_op * sexpr
  | SMethodCall of sexpr * string * sexpr list
  | SMap of sexpr * sexpr       
  | SFilter of sexpr * sexpr    
  | SReduce of sexpr * sexpr * sexpr

and sstmt =
  | SIf of sexpr * sstmt list
  | SElif of sexpr * sstmt list
  | SElse of sstmt list
  | SWhile of sexpr * sstmt list
  | SFor of sexpr * sexpr * sstmt list
  | SExprStmt of sexpr
  | SReturn of sexpr
  | SDecl of typ * string
  | SDeclAssign of typ * string * sexpr
  | SIDecl of string
  | SIDeclAssign of string * sexpr
  | SFdecl of sfdecl
  | SAssign of string * sexpr
  | STry of sstmt list * sexn_clause list * sstmt list option

and sexn_clause = {
  sexn_type: typ option;
  sexn_var: string option;
  shandler: sstmt list;
}

and sfdecl = {
  srtyp: typ;
  sfname: string;
  sparameters: bind list;
  sstmts: sstmt list;
}

(* Program type, consisting of variable declarations and functions *)
type sprogram = sstmt list

(* Pretty-printing functions *)
let string_of_sindent (curr_indent) =
  let rec append_stab indent_left =
    if indent_left <= 0 then 
      ""
    else
      "    " ^ append_stab (indent_left - 1)
  in
  append_stab curr_indent

let rec string_of_sexpr = function
    SLitInt(l) -> string_of_int l
  | SLitBool(b) -> string_of_bool b
  | SLitFloat(f) -> string_of_float(f)
  | SLitChar(c) -> "\'" ^ String.make 1 c ^ "\'"
  | SLitString(s) -> "\"" ^ s ^ "\""
  | SId(s) -> s
  | SBinop(e1, o, e2) -> string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SListExpr(el) -> "[" ^ String.concat ", " (List.map string_of_sexpr el) ^ "]"
  | SUnaryOp(op, e) -> string_of_unary_op op ^ string_of_sexpr e
  | SMethodCall(e,s,el) -> string_of_sexpr e ^ "." ^ s ^ string_of_sexpr (SListExpr(el))
  | SMap(list, func) -> string_of_sexpr list ^ " => " ^ string_of_sexpr func
  | SFilter(list, predicate) -> string_of_sexpr list ^ " =>? " ^ string_of_sexpr predicate
  | SReduce(list, func, init) -> string_of_sexpr list ^ " =>/ " ^ string_of_sexpr func ^ " with " ^ string_of_sexpr init
  | SListLit(el) -> "[" ^ String.concat ", " (List.map string_of_sexpr el) ^ "]"
  | SListBinop(e1, op, e2) -> string_of_sexpr e1 ^ " " ^ string_of_list_op op ^ " " ^ string_of_sexpr e2
  | SListInit(_, _, _) -> "ListInit" (* Added this line to handle the ListInit case *)
  (*| DeclExpr(t, s, e) -> "let " ^ s ^ " : " ^ string_of_typ t ^ " = " ^ string_of_expr e*)

let rec string_of_sstmt_list (sstmts: sstmt list) (curr_indent) = 
  String.concat "" (List.map (fun local_sstmt -> string_of_sstmt local_sstmt curr_indent) sstmts)

and string_of_sstmt (stmt) (curr_indent) = 
  string_of_sindent curr_indent ^
  match stmt with
  | SExprStmt(expr) -> string_of_sexpr expr ^ "\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ "\n"
  | SIf(e, s) -> "if (" ^ string_of_sexpr e ^ "):\n" ^ string_of_sstmt_list s (curr_indent + 1)
  | SElif(e, s) -> "elif (" ^ string_of_sexpr e ^ "):\n" ^ string_of_sstmt_list s (curr_indent + 1)
  | SElse(s) -> "else:\n" ^ string_of_sstmt_list s (curr_indent + 1)
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ "):\n" ^ string_of_sstmt_list s (curr_indent + 1)
  | SFor(e1, e2, s) -> "for (" ^ string_of_sexpr e1 ^ ", " ^ string_of_sexpr e2 ^ "):\n" ^ string_of_sstmt_list s (curr_indent + 1)
  | SDecl(t, s) -> "let " ^ s ^ " : " ^ string_of_typ t ^ "\n"
  | SDeclAssign(t, s, e) -> "let " ^ s ^ " : " ^ string_of_typ t ^ " = " ^ string_of_sexpr e ^ "\n"
  | SIDecl(s) -> "let " ^ s ^ "\n"
  | SIDeclAssign(s, e) -> "let " ^ s ^ " = " ^ string_of_sexpr e ^ "\n"
  | SFdecl(f) -> string_of_sfdecl f (curr_indent)
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e ^ "\n"
  | STry(try_block, exn_clauses, finally_block) ->
    "try:\n" ^
    string_of_sstmt_list try_block (curr_indent + 1) ^
    String.concat "" (List.map (string_of_sexn_clause (curr_indent + 1)) exn_clauses) ^
    (match finally_block with
    | Some(fb) -> "finally:\n" ^ string_of_sstmt_list fb (curr_indent + 1)
    | None -> "")

and string_of_sexn_clause indent sexn_clause =
  string_of_sindent indent ^
  "except " ^
  (match sexn_clause.sexn_type with
    | Some(t) -> "(" ^ string_of_int t ^ " " ^ (match sexn_clause.sexn_var with Some(v) -> v | None -> "") ^ ") "
    | None -> "") ^
  ":\n" ^ string_of_sstmt_list sexn_clause.shandler (indent + 1)

and string_of_sfdecl sfdecl curr_indent =
  "def " ^ sfdecl.sfname ^ "(" ^ 
  String.concat ", " (List.map (fun (t, id) ->  id ^ " : " ^ string_of_typ t) sfdecl.sparameters) ^ 
  ")" ^ " -> " ^ string_of_typ sfdecl.srtyp ^ ":" ^ "\n" ^
  String.concat "" (List.map (fun local_sstmt -> string_of_sstmt local_sstmt (curr_indent + 1)) sfdecl.sstmts)
  ^ "\n"

let string_of_sprogram (sstmts : sstmt list) =
  "\n\nParsed program: \n\n" ^
  string_of_sstmt_list sstmts 0