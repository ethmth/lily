(* Abstract Syntax Tree and functions for printing it *)


type value =
    ValInt of int
  | ValBool of bool
  | ValFloat of float
  | ValChar of char
  | ValString of string

type toke = 
 StringToken of string 
 | StringAndValueToken of string * value

type program = toke list

(* Pretty-printing functions *)
let string_of_value = function
  ValInt(x) -> string_of_int x 
  | ValBool(b) -> string_of_bool b
  | ValFloat(f) -> string_of_float f
  | ValString(s) -> s
  | ValChar(c) -> String.make 1 c

let rec string_of_token(t: toke): string = 
  match t with
  | StringToken(s) -> s ^ "\n"
  | StringAndValueToken(s, v) -> s ^ string_of_value v ^ "\n"

let string_of_program (tokens : program) =
    "\n\nLexxed program: \n\n" ^
    String.concat "" (List.map string_of_token tokens)