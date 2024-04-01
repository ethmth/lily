(* Author(s): Michaela Gary *)
(* Last Edited: April 1, 2024 *)
(* Ocamllex scanner for LILY *)

{ open Parser }

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| '\n' { token lexbuf }
| "#"     { comment lexbuf }           (* Comments *)

(* Seperators *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }

(* Punctuation *)
| ':'      { COLON }
| ','      { COMMA }
| '.'      { DECIMAL }
| "->"     { ARROW }

(* Binary Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }

(* Assignment Operators *)
| '='      { ASSIGN }

(* Relational Operators *)
| "=="     { EQ }
| "!="     { NEQ }
| '>'      { GT }
| '<'      { LT }
| ">="     { GEQ }
| "<="     { LEQ }

(* Functional Operators *)
| "=>"     { MAP }
| "=>?"    { FILTER }
| "=>/"    { REDUCE }

(* Logical Operators *)
| "&&"     { AND }
| "||"     { OR }
| '!'      { NOT }

(* Control Flow *)
| "if"       { IF }
| "is"       { IS }
| "in"       { IN }
| "none"     { NONE }
| "else"     { ELSE }
| "elif"     { ELIF }
| "for"      { FOR }
| "try"      { TRY }
| "catch"    { CATCH }
| "while"    { WHILE }
| "finally"  { FINALLY }
| "break"    { BREAK }
| "return"   { RETURN }
| "continue" { CONTINUE }

(* Declaration *)
| "const"       { CONST }
| "def"         { DEF }
| "let"         { LET }

(* Types *)
| "bool"   { BOOL }
| "int"    { INT }
| "float"  { FLOAT }
| "char"   { CHAR }
| "string" { STRING }

(* Literals *)
| "True"   { BOOL_LIT(true)  }
| "False"  { BOOL_LIT(false) }
| digit+ as lem  { INT_LIT(int_of_string lem) }
| digit+ '.' digit+ as lem { FLOAT_LIT(float_of_string lem) }
| '\'' (alpha | digit) '\'' as lem { CHAR_LIT(lem.[0]) }
| '\"' ((alpha | digit)* as lem) '\"' { STRING_LIT(lem) }
| alpha (alpha | digit | '_')* as lem { ID(lem) }

(* Miscellaneous *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }
