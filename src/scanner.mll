(* Author(s): Michaela Gary *)
(* Last Edited: April 1, 2024 *)
(* Ocamllex scanner for LILY *)

{ open Parser

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
let schar = [' ' '!' '#' '$' '%' '&' '(' ')' '*' '+' ',' '-' '.' '/' '[' ']']
let ident = "\t" | "    "

rule token = parse
(* New Line (with indentation afterwards) *)
| ['\n' '\t' ' ' '\r']* '\n' ['\n' '\t' ' ' '\r']* '#' { comment lexbuf }
| ['\n' '\t' ' ' '\r']* '\n' (ident* as ident_str) { NEWLINEI(calc_ind_size ident_str) }

| [' ' '\t' '\r'] { token lexbuf } (* Whitespace *)
| "#"     { comment lexbuf }           (* Comments *)

(* Separators *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }

(* Punctuation *)
| ':'      { COLON }
| "::"    { COLON_COLON }
| ','      { COMMA }
| '.'      { DOT }
| "->"     { ARROW }

(* Binary Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }

(* Assignment Operators *)
| '='      { ASSIGN }
| "++"     { INCREMENT }
| "--"     { DECREMENT }

(* Relational Operators *)
| "=="     { EQ }
| "!="     { NEQ }
| '>'      { GT }
| '<'      { LT }
| ">="     { GEQ }
| "<="     { LEQ }

(* Logical Operators *)
| "&&"      { AND }
| "and"     { AND }
| "or"     { OR }
| "||"      { OR}
| "!"      { NOT }
| "not"   { NOT }

(* Control Flow *)
| "if"       { IF }
| "in"       { IN }
| "else"     { ELSE }
| "for"      { FOR }
| "while"    { WHILE }
| "return"   { RETURN }

(* Declaration *)
| "def"         { DEF }
| "let"         { LET }
| "new"       { NEW }

(* Types *)
| "bool"   { BOOL }
| "int"    { INT }
| "float"  { FLOAT }
| "char"   { CHAR }
| "list"  { LIST }
| "void"   { VOID }

| "null"   { NULL }

(* Literals *)
| "True"   { BOOL_LIT(true)  }
| "False"  { BOOL_LIT(false) }
| "true"   { BOOL_LIT(true)  }
| "false"  { BOOL_LIT(false) }
| digit+ as lem  { INT_LIT(int_of_string lem) }
| digit+ '.' digit+ as lem { FLOAT_LIT(float_of_string lem) }
| '\'' (alpha | digit | schar) '\'' as lem { CHAR_LIT(lem.[1]) }
| alpha (alpha | digit | '_')* as lem { ID(lem) }

(* Miscellaneous *)
| eof { EOF }
| _ as char { raise (Failure("Scanner Error: Illegal character " ^ Char.escaped char)) }

and comment = parse
  ['\n' '\t' ' ' '\r']* '#' { comment lexbuf }
| ['\n' '\t' ' ' '\r']* '\n' (ident* as ident_str) { NEWLINEI(calc_ind_size ident_str) }
| eof { EOF }
| _    { comment lexbuf }
