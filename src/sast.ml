(* Author: Michaela Gary, Ethan Thomas *)
(* Last Edited: May 6, 2024 *)
(* Semantically-checked Abstract Syntax Tree and functions for printing it in Lily *)
open Libparser
open Ast

type sbind = typ * string * string
type sexpr = typ * expr_detail
and expr_detail =
    SLitInt of int
  | SLitBool of bool
  | SLitFloat of float
  | SLitChar of char
  | SId of string * string
  | SBinop of sexpr * op * sexpr
  | SCall of string * sexpr list * string
  | SUnaryOp of unary_op * sexpr

and sblock = SBlock of sstmt list

and sstmt =
  | SIf of sexpr * sblock * sblock
  | SWhile of sexpr * sblock
  | SFor of sexpr * sstmt * sblock
  | SExprStmt of sexpr
  | SReturn of sexpr
  | SDecl of typ * string * string
  | SDeclAssign of typ * string * sexpr * string
  | SFdecl of typ * string * sbind list * sblock * string
  | SAssign of string * sexpr * string

type sprogram = sblock

let rec string_of_sexpr (t, e) =
  "{" ^
  ( match e with 
    SLitInt(l) -> string_of_int l
  | SLitBool(b) -> string_of_bool b
  | SLitFloat(f) -> string_of_float(f)
  | SLitChar(c) -> "\'" ^ String.make 1 c ^ "\'"
  | SId(s, cname) -> "<" ^ s ^ " as " ^ cname ^ ">"
  | SBinop(e1, o, e2) -> string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SCall(f, el, cname) -> "<" ^ f ^ " as " ^ cname ^ ">" ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SUnaryOp(op, e) -> string_of_unary_op op ^ string_of_sexpr e
  ) ^ " is " ^ string_of_typ t ^ "}"
  
let rec string_of_sstmt_list (stmts: sstmt list) (curr_indent) = 
  String.concat "" (List.map (fun local_stmt -> string_of_sstmt local_stmt curr_indent) stmts)

and string_of_sstmt (sstmt) (curr_indent) = 
  string_of_indent curr_indent ^
  match sstmt with
  | SExprStmt(expr) -> string_of_sexpr expr ^ "\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ "\n"
  | SIf(e, b1, b2) -> "if (" ^ string_of_sexpr e ^ "):\n" ^ string_of_sblock b1 (curr_indent + 1) ^ (match b2 with SBlock(sl) -> if sl == [] then "" else string_of_indent curr_indent ^ "else:\n" ^ string_of_sblock b2 (curr_indent + 1))
  | SWhile(e, b) -> "while (" ^ string_of_sexpr e ^ "):\n" ^ string_of_sblock b (curr_indent + 1)
  | SFor(e,s,b) -> "for (" ^ string_of_sexpr e ^ ", " ^ (match s with SAssign(v, e, _) -> v ^ " = " ^ string_of_sexpr e | _ -> "STATEMENT") ^ "):\n" ^ string_of_sblock b (curr_indent + 1)
  | SDecl(t, s, cname) -> "let " ^ "<" ^ s ^ " as " ^ cname ^ ">" ^ " : " ^ string_of_typ t ^ "\n"
  | SDeclAssign(t, s, e, cname) -> "let " ^ "<" ^ s ^ " as " ^ cname ^ ">" ^ " : " ^ string_of_typ t ^ " = " ^ string_of_sexpr e ^ "\n"
  | SAssign(v, e, cname) -> "<" ^ v ^ " as " ^ cname ^ ">" ^ " = " ^ string_of_sexpr e ^ "\n"
  | SFdecl(t, s, p, b, cname) -> "def " ^ "<" ^ s ^ " as " ^ cname ^ ">" ^ "(" ^ 
      String.concat ", " (List.map (fun (t, id, cname) ->  "<" ^ id ^ " as " ^ cname ^ ">" ^ " : " ^ string_of_typ t) p) ^ 
      ")" ^ " -> " ^ string_of_typ t ^ ":" ^ "\n"
      ^ string_of_sblock b (curr_indent + 1)

and string_of_sblock (block: sblock) (indent: int): string =
  match block with
  SBlock(sl) -> string_of_sstmt_list sl indent

let string_of_sprogram (blk : sblock) =
    "\n\nParsed program: \n\n" ^
    string_of_sblock blk 0
