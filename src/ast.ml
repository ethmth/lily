(* Author(s): Michaela Gary, Ethan Thomas, Tani Omoyeni, Chimaobi Onwuka, Jianje Sun *)
(* Last Edited: April 25, 2024 *)
(* Abstract Syntax Tree and functions for printing it *)

(* Operators definition *)
(* TODO: Add back list operations *)
type op = Plus | Minus | Times | Divide | Eq | Neq | Lt | Leq | Gt | Geq | And | Or (* | Map | Filter | Reduce *)

(* Unary operator definition *)
type unary_op = Negate

(* Types definition *)
(* TODO: Add back string *)
type typ = Int | Bool | Char | Float | Void | List of typ | Any (* Any is only for internal use, DON'T SCAN THIS *) (* | String *) 

type bind = typ * string

(* Definition of list operations *)
type list_op = 
  | ElwiseAdd

(* Expressions definition *)
(* TODO: Add back string *)
type expr =
    LitInt of int
  | LitBool of bool
  | LitFloat of float
  | LitChar of char
  (* | LitString of string *)
  | LitList of expr list
  | Id of string
  | Binop of expr * op * expr
  | Call of string * expr list
  | UnaryOp of unary_op * expr
  | ListIndex of string * expr
  (* TODO: Implement ListNew function *)
  (* | ListNew of typ * int *)
  | Assign of string * expr

and block = Block of stmt list

and stmt =
  (* TODO: Add support for elifs? (Probably should be one of last features we add) *)
  | If of expr * block * block
  (* | If of expr * block * (expr * block) list * block *)
  | While of expr * block
  | For of expr * expr * block
  | ExprStmt of expr
  | Return of expr
  | Decl of typ * string
  | DeclAssign of typ * string * expr
  | Fdecl of typ * string * bind list * block (* (rtyp, fname, parameters, stmts) *)
  (* TODO: Add back try/except statements (we don't have a way of throwing errors, so this is useless rn AND VERY HARD TO IMPLEMENT well) *)
  (* | Try of stmt list * exn_clause list * stmt list option *)

(* and exn_clause = {
  exn_type: typ option;
  exn_var: string option;
  handler: stmt list;
} *)

(* Program type, consisting of variable declarations and functions *)
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
  (* | Map -> "=>"        *)
  (* | Filter -> "=>?"    *)
  (* | Reduce -> "=>/"   *)

let string_of_unary_op = function
    Negate -> "!"

let string_of_list_op = function
  | ElwiseAdd -> ".+" (*CHIMA NEW: Added this line*)

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  (*| String -> "string"*) (*CHIMA NEW: Added this line*)
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
  (* | LitString(s) -> "\"" ^ s ^ "\"" *)
  | LitList(el) -> string_of_list el
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | UnaryOp(op, e) -> string_of_unary_op op ^ string_of_expr e
  | ListIndex(id, e) -> id ^ "[" ^ string_of_expr e ^ "]"
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e ^ "\n"
  (* | Map(list, func) -> string_of_expr list ^ " => " ^ string_of_expr func *)
  (* | Filter(list, predicate) -> string_of_expr list ^ " =>? " ^ string_of_expr predicate *)
  (* | Reduce(list, func, init) -> string_of_expr list ^ " =>/ " ^ string_of_expr func ^ " with " ^ string_of_expr init *)
  
let rec string_of_stmt_list (stmts: stmt list) (curr_indent) = 
  String.concat "" (List.map (fun local_stmt -> string_of_stmt local_stmt curr_indent) stmts)

and string_of_stmt (stmt) (curr_indent) = 
  string_of_indent curr_indent ^
  match stmt with
  | ExprStmt(expr) -> string_of_expr expr ^ "\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ "\n"
  | If(e, b1, b2) -> "if (" ^ string_of_expr e ^ "):\n" ^ string_of_block b1 (curr_indent + 1) ^ (match b2 with Block(sl) -> if sl == [] then "" else string_of_indent curr_indent ^ "else:\n" ^ string_of_block b2 (curr_indent + 1))
  (* | Elif(e, s) -> "elif (" ^ string_of_expr e ^ "):\n" ^ string_of_stmt_list s (curr_indent + 1) *)
  (* | Else(s) -> "else:\n" ^ string_of_stmt_list s (curr_indent + 1) *)
  | While(e, b) -> "while (" ^ string_of_expr e ^ "):\n" ^ string_of_block b (curr_indent + 1)
  | For(e,a,b) -> "for (" ^ string_of_expr e ^ ", " ^ string_of_expr a ^ "):\n" ^ string_of_block b (curr_indent + 1)
  | Decl(t, s) -> "let " ^ s ^ " : " ^ string_of_typ t ^ "\n"
  | DeclAssign(t, s, e) -> "let " ^ s ^ " : " ^ string_of_typ t ^ " = " ^ string_of_expr e ^ "\n"
  (* | Try(try_block, exn_clauses, finally_block) ->
    "try:\n" ^
    string_of_stmt_list try_block (curr_indent + 1) ^
    String.concat "" (List.map (string_of_exn_clause (curr_indent + 1)) exn_clauses) ^
    (match finally_block with
     | Some(fb) -> "finally:\n" ^ string_of_stmt_list fb (curr_indent + 1)
     | None -> "") *)
  | Fdecl(t, s, p, b) -> "def " ^ s ^ "(" ^ 
      String.concat ", " (List.map (fun (t, id) ->  id ^ " : " ^ string_of_typ t) p) ^ 
      ")" ^ " -> " ^ string_of_typ t ^ ":" ^ "\n"
      ^ string_of_block b (curr_indent + 1)

(* and string_of_exn_clause indent exn_clause =
  string_of_indent indent ^
  "except " ^
  (match exn_clause.exn_type with
   | Some(t) -> "(" ^ string_of_typ t ^ " " ^ (match exn_clause.exn_var with Some(v) -> v | None -> "") ^ ") "
   | None -> "") ^
  ":\n" ^ string_of_stmt_list exn_clause.handler (indent + 1) *)

and string_of_block (block: block) (indent: int): string =
  match block with
  Block(sl) -> string_of_stmt_list sl indent

let string_of_program (blk : block) =
    "\n\nParsed program: \n\n" ^
    string_of_block blk 0
