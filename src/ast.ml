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
  | Assign of string * expr
  | Call of string * expr list
  | ListExpr of expr list
  | UnaryOp of unary_op * expr
  (* | ListLit of expr list  Chima New: Represents list literals *)
  | MethodCall of expr * string * expr list  (* Chima New: Represents method calls on expressions *)


(* Statements definition *)
type stmt =
  Block of stmt list
  | If of expr * stmt list * stmt list (*option  Modified: Allow for optional else branch *)
  | While of expr * stmt list
  | Expr of expr
  | Return of expr
  | Decl of typ * string
  | Fdecl of fdecl

(* Function declaration type *)
and fdecl = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  (* locals: (typ * string) list; *) (* TODO Not sure if we're using locals?*)
  body: stmt list;
}

(* Program type, consisting of variable declarations and functions *)
(* type program = (typ * string) list * fdecl list *)
type program = stmt list

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
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
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
    Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt_list s1 ^ "else\n" ^ string_of_stmt_list s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt_list s
  | Decl(t, s) -> s ^ " : " ^ string_of_typ t ^ "\n"
  | Fdecl(f) -> string_of_fdecl f

(* let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n" *)

and string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^ fdecl.fname ^ "(" ^ 
  String.concat ", " (List.map (fun (t, id) -> string_of_typ t ^ " " ^ id) fdecl.formals) ^ ")\n{\n" ^
  (* String.concat "" (List.map string_of_vdecl fdecl.locals) ^ *) (*TODO Not sure if we're using locals?*)
  String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"

(* let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^
  String.concat "\n" (List.map string_of_fdecl funcs) *)

  let string_of_program (stmts : stmt list) =
    "\n\nParsed program: \n\n" ^
    string_of_stmt_list stmts