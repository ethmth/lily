type token =
  | NEWLINE
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COLON
  | COMMA
  | ARROW
  | DOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | EQ
  | NEQ
  | GT
  | LT
  | GEQ
  | LEQ
  | MAP
  | FILTER
  | REDUCE
  | AND
  | OR
  | NOT
  | IF
  | IS
  | IN
  | NONE
  | ELSE
  | ELIF
  | FOR
  | TRY
  | EXCEPT
  | WHILE
  | FINALLY
  | BREAK
  | RETURN
  | CONTINUE
  | CONST
  | DEF
  | LET
  | BOOL
  | INT
  | FLOAT
  | CHAR
  | STRING
  | INT_LIT of (int)
  | FLOAT_LIT of (float)
  | BOOL_LIT of (bool)
  | CHAR_LIT of (char)
  | STRING_LIT of (string)
  | ID of (string)
  | EOF
  | NEWLINEI of (int)
  | INDENT
  | DEDENT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
