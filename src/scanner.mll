(* Ocamllex scanner for LILY *)

(* { open parser } *)

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
|   [' ' '\t' '\r' '\n']   { token lexbuf }
|   "/*"        { comment lexbuf }
|   "int"       { INT }
|   "bool"      { BOOL } 
|   "true"      { BLIT(true) }
|   "false"     { BLIT(false) }
|   '('         { LPAREN }
|   ')'         { RPAREN }
|   '{'         { LBRACE }
|   '}'         { RBRACE }
|   ';'         { SEMI }
|   ','         { COMMA }
|   '+'         { PLUS }
|   '-'         { MINUS }
|   '='         { ASSIGN }
|   "=="        { EQ }
|   "!="        { NEQ }
|   '<'         { LT }
|   "&&"        { AND }
|   "||"        { OR }
|   "if"        { IF }
|   "else"      { ELSE }
|   "while"     { WHILE }
|   "return"    { RETURN }
| digit+ as lem      { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem     { ID(lem) }
| eof           {EOF}
|   _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
| "*/"      { token lexbuf }
| _         { comment lexbuf }