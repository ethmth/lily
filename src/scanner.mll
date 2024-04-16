(* Author(s): Michaela Gary *)
(* Last Edited: April 1, 2024 *)
(* Ocamllex scanner for LILY *)

{ open Parser
(* UNCOMMENT THIS FOR test0: *)
(* { open Parserscanner  *)

  let calc_ind_size (ident_str: string) =
    let cur_size = ref 0 in
    String.iter (fun c ->
    if c = '\t' then
      cur_size := !cur_size + 4
    else
      cur_size := !cur_size + 1
    ) ident_str;
    (!cur_size / 4)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let schar = [' ' '!' '#' '$' '%' '&' '(' ')' '*' '+' ',' '-' '.' '/']
let ident = "\t" | "    "

rule token = parse
(* New Line (with indentation afterwards) *)
| ['\n' '\t' ' ' '\r']* '\n' ['\n' '\t' ' ' '\r']* '#' { comment lexbuf }
| ['\n' '\t' ' ' '\r']* '\n' (ident* as ident_str) { NEWLINEI(calc_ind_size ident_str) }

| [' ' '\t' '\r'] { token lexbuf } (* Whitespace *)
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
| '.'      { DOT }
| "->"     { ARROW }

(* Binary Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
(* TODO: Add exponent? (**) *)

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

(* Additional functional operator for REDUCE *)
| "with"   { WITH }

(* Control Flow *)
| "if"       { IF }
| "is"       { IS }
| "in"       { IN }
| "none"     { NONE }
| "else"     { ELSE }
| "elif"     { ELIF }
| "for"      { FOR }
| "try"      { TRY }
| "except"    { EXCEPT }
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
| '\'' (alpha | digit | schar) '\'' as lem { CHAR_LIT(lem.[0]) }
| '\"' ((alpha | digit | schar)* as lem) '\"' { STRING_LIT(lem) }
| alpha (alpha | digit | '_')* as lem { ID(lem) }

(* Miscellaneous *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  ['\n' '\t' ' ' '\r']* '#' { comment lexbuf }
| ['\n' '\t' ' ' '\r']* '\n' (ident* as ident_str) { NEWLINEI(calc_ind_size ident_str) }
| eof { EOF }
| _    { comment lexbuf }
