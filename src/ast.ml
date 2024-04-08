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
  | Expr of expr
  | Return of stmt
  | Decl of typ * string
  | Fdecl of fdecl
  | Assign of string * stmt

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
  | LitChar(c) -> String.make 1 c
  | LitString(s) -> s
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

let rec string_of_stmt_list (stmts: stmt list) = 
  String.concat "" (List.map string_of_stmt stmts)

and string_of_stmt = function
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_stmt expr ^ ";\n"
  | If(e, s) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt_list s
  | Elif(e, s) -> "elif (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt_list s
  | Else(s) -> "else\n" ^ string_of_stmt_list s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt_list s
  | For(e1,e2,s) -> "for (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ "):\n" ^string_of_stmt_list s
  | Decl(t, s) -> s ^ " : " ^ string_of_typ t ^ "\n"
  | Fdecl(f) -> string_of_fdecl f
  | Assign(v, e) -> v ^ " = " ^ string_of_stmt e

and string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^ fdecl.fname ^ "(" ^ 
  String.concat ", " (List.map (fun (t, id) -> string_of_typ t ^ " " ^ id) fdecl.parameters) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.stmts) ^ "}\n"
let string_of_program (stmts : stmt list) =
    "\n\nParsed program: \n\n" ^
    string_of_stmt_list stmts