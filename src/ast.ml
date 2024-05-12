(* Author(s): Michaela Gary, Ethan Thomas, Tani Omoyeni, Chimaobi Onwuka, Jianje Sun *)
(* Last Edited: April 25, 2024 *)
(* Abstract Syntax Tree and functions for printing it *)

(* Operators definition *)
type op = Plus | Minus | Times | Divide | Eq | Neq | Lt | Leq | Gt | Geq | And | Or

(* Unary operator definition *)
type unary_op = Negate

(* Types definition *)
type typ = Int | Bool | Char | Float | Void | List of typ | Any

type bind = typ * string

(* Definition of list operations *)
type list_op = 
  | ElwiseAdd

(* Expressions definition *)
type expr =
    LitInt of int
  | LitBool of bool
  | LitFloat of float
  | LitChar of char
  | LitList of expr list
  | Id of string
  | Binop of expr * op * expr
  | Call of string * expr list
  | UnaryOp of unary_op * expr
  | ListIndex of string * expr
  | Assign of string * expr
  | AssignIndex of string * expr * expr
  | NewList of typ * expr
  | Null

and block = Block of stmt list

and stmt =
  | If of expr * block * block
  | While of expr * block
  | For of expr * expr * block
  | ForIn of string * expr * block
  | ExprStmt of expr
  | Return of expr
  | Decl of typ * string
  | DeclAssign of typ * string * expr
  | Fdecl of typ * string * bind list * block

type program = block

let string_of_op = function
  Plus -> "+" 
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/" 
  | Eq -> "==" 
  | Neq -> "!=" 
  | Lt -> "<" 
  | Leq -> "<="
  | Gt -> ">" 
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"

let string_of_unary_op = function
    Negate -> "!"

let string_of_list_op = function
  | ElwiseAdd -> ".+" 

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | Void -> "void"
  | Any -> "any"
  | List(t) -> ((string_of_typ t) ^ " list")

(* Pretty-printing functions *)
let string_of_indent (curr_indent) =
  let rec append_tab indent_left =
    if indent_left <= 0 then 
      ""
    else
      "    " ^ append_tab (indent_left - 1)
  in
  append_tab curr_indent

let rec string_of_list (exprs: expr list): string =
  "[" ^ String.concat ", " (List.map string_of_expr exprs) ^ "]"

and string_of_expr = function
    LitInt(l) -> string_of_int l
  | LitBool(b) -> string_of_bool b
  | LitFloat(f) -> string_of_float(f)
  | LitChar(c) -> "\'" ^ String.make 1 c ^ "\'"
  | LitList(el) -> string_of_list el
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | UnaryOp(op, e) -> string_of_unary_op op ^ string_of_expr e
  | ListIndex(id, e) -> id ^ "[" ^ string_of_expr e ^ "]"
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e ^ "\n"
  | AssignIndex(v, ind, e) -> v ^ "[" ^ string_of_expr ind ^ "]" ^ " = " ^ string_of_expr e ^ "\n"
  | NewList( t, size) -> "new " ^ (string_of_typ t) ^ "[" ^ (string_of_expr size) ^ "]"
  | Null -> "null"
  
let rec string_of_stmt_list (stmts: stmt list) (curr_indent) = 
  String.concat "" (List.map (fun local_stmt -> string_of_stmt local_stmt curr_indent) stmts)

and string_of_stmt (stmt) (curr_indent) = 
  string_of_indent curr_indent ^
  match stmt with
  | ExprStmt(expr) -> string_of_expr expr ^ "\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ "\n"
  | If(e, b1, b2) -> "if (" ^ string_of_expr e ^ "):\n" ^ string_of_block b1 (curr_indent + 1) ^ (match b2 with Block(sl) -> if sl == [] then "" else string_of_indent curr_indent ^ "else:\n" ^ string_of_block b2 (curr_indent + 1))
  | While(e, b) -> "while (" ^ string_of_expr e ^ "):\n" ^ string_of_block b (curr_indent + 1)
  | For(e,a,b) -> "for (" ^ string_of_expr e ^ ", " ^ string_of_expr a ^ "):\n" ^ string_of_block b (curr_indent + 1)
  | ForIn(id,e,b) -> "for " ^ id ^ " in " ^ string_of_expr e ^ ":\n" ^ string_of_block b (curr_indent + 1)
  | Decl(t, s) -> "let " ^ s ^ " : " ^ string_of_typ t ^ "\n"
  | DeclAssign(t, s, e) -> "let " ^ s ^ " : " ^ string_of_typ t ^ " = " ^ string_of_expr e ^ "\n"
  | Fdecl(t, s, p, b) -> "def " ^ s ^ "(" ^ 
      String.concat ", " (List.map (fun (t, id) ->  id ^ " : " ^ string_of_typ t) p) ^ 
      ")" ^ " -> " ^ string_of_typ t ^ ":" ^ "\n"
      ^ string_of_block b (curr_indent + 1)

and string_of_block (block: block) (indent: int): string =
  match block with
  Block(sl) -> string_of_stmt_list sl indent

let string_of_program (blk : block) =
    "\n\nParsed program: \n\n" ^
    string_of_block blk 0
