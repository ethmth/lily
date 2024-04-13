(* Abstract Syntax Tree and functions for printing it *)

(* Operators definition *)
type op = Plus | Minus | Times | Divide | Eq | Neq | Lt | Leq | Gt | Geq

(* Unary operator definition *)
type unary_op = Negate

(* Types definition *)
type typ = Int | Bool | Char | Float | String

type bind = typ * string

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
  (* | ListLit of expr list  Chima New: Represents list literals *)
  | MethodCall of expr * string * expr list  (* Chima New: Represents method calls on expressions *)


(* Statements definition *)
type stmt =
  | If of expr * stmt list
  | Elif of expr * stmt list
  | Else  of stmt list
  | While of expr * stmt list
  | For of expr * expr * stmt list
  | ExprStmt of expr
  | Return of expr
  | Decl of typ * string
  | Fdecl of fdecl
  | Assign of string * expr

(* Function declaration type *)
and fdecl = {
  rtyp: typ;
  fname: string;
  parameters: bind list;
  stmts: stmt list;
}

(* Program type, consisting of variable declarations and functions *)
type program = stmt list

(* TODO (Ethan) Fix pretty printing questions *)

(* Pretty-printing functions *)
let string_of_indent (curr_indent) =
  let rec append_tab indent_left =
    if indent_left <= 0 then 
      ""
    else
      "    " ^ append_tab (indent_left - 1)
  in
  append_tab curr_indent

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

let string_of_unary_op = function
    Negate -> "!"

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

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | String -> "string"

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
  | Fdecl(f) -> string_of_fdecl f (curr_indent)
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e ^ "\n"

and string_of_fdecl fdecl curr_indent =
  "def " ^ fdecl.fname ^ "(" ^ 
  String.concat ", " (List.map (fun (t, id) ->  id ^ " : " ^ string_of_typ t) fdecl.parameters) ^ 
  ")" ^ " -> " ^ string_of_typ fdecl.rtyp ^ ":" ^ "\n" ^
  String.concat "" (List.map (fun local_stmt -> string_of_stmt local_stmt (curr_indent + 1)) fdecl.stmts)
  ^ "\n"

let string_of_program (stmts : stmt list) =
    "\n\nParsed program: \n\n" ^
    string_of_stmt_list stmts 0