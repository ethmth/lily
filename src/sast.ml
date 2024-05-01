(* Author: Michaela Gary *)
(* Last Edited: May 1, 2024 *)
(* Semantically-checked Abstract Syntax Tree and functions for printing it in Lily *)

open Ast

(* Operators definition *)
type sop = SPlus | SMinus | STimes | SDivide | SEq | SNeq | SLt | SLeq | SGt | SGeq | SMap | SFilter | SReduce

(* Unary operator definition *)
type sunary_op = SNegate

(* Types definition *)
type styp = SInt | SBool | SChar | SFloat | SString | SList of styp

type sbind = styp * string

(* Definition of list operations *)
type slist_op = 
  | SElwiseAdd

type sexpr = styp * expr_detail
and expr_detail =
    SLitInt of int
  | SLitBool of bool
  | SLitFloat of float
  | SLitChar of char
  | SLitString of string
  | SId of string
  | SBinop of sexpr * sop * sexpr
  | SCall of string * sexpr list
  | SListExpr of sexpr list
  | SUnaryOp of sunary_op * sexpr
  | SListLit of sexpr list 
  | SListInit of string * styp * sexpr list
  (* | SDeclExpr of styp * string * sexpr *)
  | SListBinop of sexpr * slist_op * sexpr
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
  | SDecl of styp * string
  | SDeclAssign of styp * string * sexpr
  | SIDecl of string
  | SIDeclAssign of string * sexpr
  | SFdecl of sfdecl
  | SAssign of string * sexpr
  | STry of sstmt list * sexn_clause list * sstmt list option

and sexn_clause = {
  sexn_type: styp option;
  sexn_var: string option;
  shandler: sstmt list;
}

and sfdecl = {
  srtyp: styp;
  sfname: string;
  sparameters: sbind list;
  sstmts: sstmt list;
}

let string_of_sop = function
    SPlus -> "+" 
  | SMinus -> "-"
  | STimes -> "*"
  | SDivide -> "/" 
  | SEq -> "==" 
  | SNeq -> "!=" 
  | SLt -> "<" 
  | SLeq -> "<="
  | SGt -> ">" 
  | SGeq -> ">="
  | SMap -> "=>"       
  | SFilter -> "=>?"   
  | SReduce -> "=>/"  

let string_of_sunary_op = function
    SNegate -> "!"

let string_of_slist_op = function
  | SElwiseAdd -> ".+"

let string_of_styp = function
    SInt -> "int"
  | SBool -> "bool"
  | SChar -> "char"
  | SFloat -> "float"
  | SString -> "string"
  | SList _ -> "list"

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
  | SBinop(e1, o, e2) -> string_of_sexpr e1 ^ " " ^ string_of_sop o ^ " " ^ string_of_sexpr e2
  | SCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SListExpr(el) -> "[" ^ String.concat ", " (List.map string_of_sexpr el) ^ "]"
  | SUnaryOp(op, e) -> string_of_sunary_op op ^ string_of_sexpr e
  | SMethodCall(e,s,el) -> string_of_sexpr e ^ "." ^ s ^ string_of_sexpr (ListExpr(el))
  | SMap(list, func) -> string_of_sexpr list ^ " => " ^ string_of_sexpr func
  | SFilter(list, predicate) -> string_of_sexpr list ^ " =>? " ^ string_of_sexpr predicate
  | SReduce(list, func, init) -> string_of_sexpr list ^ " =>/ " ^ string_of_sexpr func ^ " with " ^ string_of_sexpr init
  | SListLit(el) -> "[" ^ String.concat ", " (List.map string_of_sexpr el) ^ "]"
  | SListBinop(e1, op, e2) -> string_of_sexpr e1 ^ " " ^ string_of_slist_op op ^ " " ^ string_of_sexpr e2
  | SListInit(_, _, _) -> "ListInit" (* Added this line to handle the ListInit case *)
  (*| SDeclExpr(t, s, e) -> "let " ^ s ^ " : " ^ string_of_styp t ^ " = " ^ string_of_sexpr e*)

let rec string_of_sstmt_list (sstmts: sstmt list) (curr_indent) = 
  String.concat "" (List.map (fun local_sstmt -> string_of_sstmt local_sstmt curr_indent) sstmts)

and string_of_sstmt (stmt) (curr_indent) = 
  string_of_sindent curr_indent ^
  match stmt with
  | SExprStmt(sexpr) -> string_of_sexpr sexpr ^ "\n"
  | SReturn(sexpr) -> "return " ^ string_of_sexpr sexpr ^ "\n"
  | SIf(se, ss) -> "if (" ^ string_of_sexpr se ^ "):\n" ^ string_of_sstmt_list ss (curr_indent + 1)
  | SElif(se, ss) -> "elif (" ^ string_of_sexpr se ^ "):\n" ^ string_of_sstmt_list ss (curr_indent + 1)
  | SElse(ss) -> "else:\n" ^ string_of_sstmt_list ss (curr_indent + 1)
  | SWhile(se, ss) -> "while (" ^ string_of_sexpr se ^ "):\n" ^ string_of_sstmt_list ss (curr_indent + 1)
  | SFor(se1, se2, ss) -> "for (" ^ string_of_sexpr se1 ^ ", " ^ string_of_sexpr se2 ^ "):\n" ^ string_of_sstmt_list ss (curr_indent + 1)
  | SDecl(t, s) -> "let " ^ s ^ " : " ^ string_of_styp t ^ "\n"
  | SDeclAssign(t, s, se) -> "let " ^ s ^ " : " ^ string_of_styp t ^ " = " ^ string_of_sexpr se ^ "\n"
  | SIDecl(s) -> "let " ^ s ^ "\n"
  | SIDeclAssign(s, se) -> "let " ^ s ^ " = " ^ string_of_sexpr se ^ "\n"
  | SFdecl(sf) -> string_of_sfdecl sf (curr_indent)
  | SAssign(v, se) -> v ^ " = " ^ string_of_sexpr se ^ "\n"
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
    | Some(t) -> "(" ^ string_of_styp t ^ " " ^ (match sexn_clause.sexn_var with Some(v) -> v | None -> "") ^ ") "
    | None -> "") ^
  ":\n" ^ string_of_sstmt_list sexn_clause.shandler (indent + 1)

and string_of_sfdecl sfdecl curr_indent =
  "def " ^ sfdecl.sfname ^ "(" ^ 
  String.concat ", " (List.map (fun (t, id) ->  id ^ " : " ^ string_of_styp t) sfdecl.sparameters) ^ 
  ")" ^ " -> " ^ string_of_styp sfdecl.srtyp ^ ":" ^ "\n" ^
  String.concat "" (List.map (fun local_sstmt -> string_of_sstmt local_sstmt (curr_indent + 1)) sfdecl.sstmts)
  ^ "\n"

  let string_of_sprogram (sstmts : sstmt list) =
  "\n\nParsed program: \n\n" ^
  string_of_sstmt_list sstmts 0