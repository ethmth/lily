(* Author(s): Michaela Gary, Ethan Thomas, Tani Omoyeni, Chimaobi Onwuka, Jianje Sun *)
(* Last Edited: April 25, 2024 *)
(* Abstract Syntax Tree and functions for printing it *)

(* Operators definition *)
type op = Plus | Minus | Times | Divide | Eq | Neq | Lt | Leq | Gt | Geq | Map | Filter | Reduce

(* Unary operator definition *)
type unary_op = Negate

(* Types definition *)
type typ = Int | Bool | Char | Float | String | List of typ

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
  | LitString of string
  | Id of string
  | Binop of expr * op * expr
  | Call of string * expr list
  | ListExpr of expr list
  | UnaryOp of unary_op * expr
  | ListLit of expr list  (*Chima New: Represents list literals*)
  | ListInit of string * typ * expr list  (*Chima New: Represents list initialization*)
  (* | DeclExpr of typ * string * expr  (* New type to treat declarations as expressions CHIMA *)*)
  | ListBinop of expr * list_op * expr   (* Adding this line *)
  | MethodCall of expr * string * expr list  (* Chima New: Represents method calls on expressions *)
  | Map of expr * expr       
  | Filter of expr * expr    
  | Reduce of expr * expr * expr  


and stmt =
  | If of expr * stmt list
  | Elif of expr * stmt list
  | Else of stmt list
  | While of expr * stmt list
  | For of expr * expr * stmt list
  | ExprStmt of expr
  | Return of expr
  | Decl of typ * string
  | DeclAssign of typ * string * expr
  | IDecl of string
  | IDeclAssign of string * expr
  | Fdecl of fdecl
  | Assign of string * expr
  | Try of stmt list * exn_clause list * stmt list option

and exn_clause = {
  exn_type: typ option;
  exn_var: string option;
  handler: stmt list;
}

and fdecl = {
  rtyp: typ;
  fname: string;
  parameters: bind list;
  stmts: stmt list;
}
(* Program type, consisting of variable declarations and functions *)
type program = stmt list  

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
  | Map -> "=>"       
  | Filter -> "=>?"   
  | Reduce -> "=>/"  

let string_of_unary_op = function
    Negate -> "!"

let string_of_list_op = function
  | ElwiseAdd -> ".+" (*CHIMA NEW: Added this line*)

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | String -> "string" (*CHIMA NEW: Added this line*)
  | List _ -> "list" (*CHIMA NEW: Added this line*)

(* Pretty-printing functions *)
let string_of_indent (curr_indent) =
  let rec append_tab indent_left =
    if indent_left <= 0 then 
      ""
    else
      "    " ^ append_tab (indent_left - 1)
  in
  append_tab curr_indent

let rec string_of_expr = function
    LitInt(l) -> string_of_int l
  | LitBool(b) -> string_of_bool b
  | LitFloat(f) -> string_of_float(f)
  | LitChar(c) -> "\'" ^ String.make 1 c ^ "\'"
  | LitString(s) -> "\"" ^ s ^ "\""
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ListExpr(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | UnaryOp(op, e) -> string_of_unary_op op ^ string_of_expr e
  | MethodCall(e,s,el) -> string_of_expr e ^ "." ^ s ^ string_of_expr (ListExpr(el))
  | Map(list, func) -> string_of_expr list ^ " => " ^ string_of_expr func
  | Filter(list, predicate) -> string_of_expr list ^ " =>? " ^ string_of_expr predicate
  | Reduce(list, func, init) -> string_of_expr list ^ " =>/ " ^ string_of_expr func ^ " with " ^ string_of_expr init
  | ListLit(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | ListBinop(e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_list_op op ^ " " ^ string_of_expr e2
  | ListInit(_, _, _) -> "ListInit" (* Added this line to handle the ListInit case *)
  (*| DeclExpr(t, s, e) -> "let " ^ s ^ " : " ^ string_of_typ t ^ " = " ^ string_of_expr e*)
  
let rec string_of_stmt_list (stmts: stmt list) (curr_indent) = 
  String.concat "" (List.map (fun local_stmt -> string_of_stmt local_stmt curr_indent) stmts)

and string_of_stmt (stmt) (curr_indent) = 
  string_of_indent curr_indent ^
  match stmt with
  | ExprStmt(expr) -> string_of_expr expr ^ "\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ "\n"
  | If(e, s) -> "if (" ^ string_of_expr e ^ "):\n" ^ string_of_stmt_list s (curr_indent + 1)
  | Elif(e, s) -> "elif (" ^ string_of_expr e ^ "):\n" ^ string_of_stmt_list s (curr_indent + 1)
  | Else(s) -> "else:\n" ^ string_of_stmt_list s (curr_indent + 1)
  | While(e, s) -> "while (" ^ string_of_expr e ^ "):\n" ^ string_of_stmt_list s (curr_indent + 1)
  | For(e1,e2,s) -> "for (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ "):\n" ^ string_of_stmt_list s (curr_indent + 1)
  | Decl(t, s) -> "let " ^ s ^ " : " ^ string_of_typ t ^ "\n"
  | DeclAssign(t, s, e) -> "let " ^ s ^ " : " ^ string_of_typ t ^ " = " ^ string_of_expr e ^ "\n"
  | IDecl(s) -> "let " ^ s ^ "\n"
  | IDeclAssign(s, e) -> "let " ^ s ^ " = " ^ string_of_expr e ^ "\n"
  | Fdecl(f) -> string_of_fdecl f (curr_indent)
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e ^ "\n"
  | Try(try_block, exn_clauses, finally_block) ->
    "try:\n" ^
    string_of_stmt_list try_block (curr_indent + 1) ^
    String.concat "" (List.map (string_of_exn_clause (curr_indent + 1)) exn_clauses) ^
    (match finally_block with
     | Some(fb) -> "finally:\n" ^ string_of_stmt_list fb (curr_indent + 1)
     | None -> "")

and string_of_fdecl fdecl curr_indent =
  "def " ^ fdecl.fname ^ "(" ^ 
  String.concat ", " (List.map (fun (t, id) ->  id ^ " : " ^ string_of_typ t) fdecl.parameters) ^ 
  ")" ^ " -> " ^ string_of_typ fdecl.rtyp ^ ":" ^ "\n" ^
  String.concat "" (List.map (fun local_stmt -> string_of_stmt local_stmt (curr_indent + 1)) fdecl.stmts)
  ^ "\n"

and string_of_exn_clause indent exn_clause =
  string_of_indent indent ^
  "except " ^
  (match exn_clause.exn_type with
   | Some(t) -> "(" ^ string_of_typ t ^ " " ^ (match exn_clause.exn_var with Some(v) -> v | None -> "") ^ ") "
   | None -> "") ^
  ":\n" ^ string_of_stmt_list exn_clause.handler (indent + 1)

let string_of_program (stmts : stmt list) =
    "\n\nParsed program: \n\n" ^
    string_of_stmt_list stmts 0
