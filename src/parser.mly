/* Author(s): Michaela Gary, Ethan Thomas, Tani Omoyeni */
/* Last Edited: April 1, 2024 */
/* Ocamllex parser for LILY */

%{
open Ast
%}

/* New Line */
%token NEWLINE

/* Separators */
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET


/* Punctuation */
%token COLON COLON_COLON COMMA ARROW
%token DOT

/* Binary Operators */
%token PLUS MINUS TIMES DIVIDE MOD

/* Assignment Operators */
%token ASSIGN
%token INCREMENT DECREMENT

/* Relational Operators */
%token EQ NEQ GT LT GEQ LEQ 

/* Logical Operators */
%token AND OR NOT

/* Control Flow */
%token IF IN ELSE FOR WHILE RETURN

/* Declaration */
%token DEF LET

/* Types */
%token BOOL INT FLOAT CHAR VOID LIST

/* Literals */
%token NULL NEW
%token <int> INT_LIT 
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <string> ID

/* Miscellaneous */
%token EOF

/* Indentation Tokens */
%token <int> NEWLINEI
%token INDENT DEDENT

%start program
%type <Ast.program> program

/* Operator Associativity */
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left GT LT GEQ LEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%%

program:
  NEWLINE block EOF { Block($2) }

block:
  statements { $1 }

statements:
  /* nothing */ { [] }
  | statement statements  { $1::$2 }

statement:
    stmt_simple NEWLINE { $1 }  // one-line statements
  | stmt_compound { $1 }        // multi-line "block" statements

stmt_simple:
    declaration { $1 }
  | expression_statement { $1 }
  | return_statement { $1 }

stmt_compound:
  function_def { $1 }
  | for_loop  { $1 }
  | while_loop  { $1 }
  | if_statement { $1 }

/* stmt_simple */
declaration: 
  /* regular declarations */
  LET ID COLON typ ASSIGN expression { DeclAssign($4, $2, $6)}
  | LET ID COLON typ { Decl($4, $2) }
  /* list declarations */
  | LET ID COLON_COLON typ ASSIGN LBRACKET RBRACKET { DeclAssign(List($4), $2, NewList($4, LitInt(0)))}
  | LET ID COLON_COLON typ ASSIGN expression { DeclAssign(List($4), $2, $6) }
  | LET ID COLON_COLON typ { Decl(List($4), $2) }

expression_statement:
  expression { ExprStmt($1) }

return_statement:
  | RETURN expression { Return($2) }

/* stmt_compound */
function_def:
  DEF ID LPAREN parameters_opt RPAREN ARROW typ COLON NEWLINE INDENT block DEDENT { Fdecl($7, $2, $4, Block($11)) }

parameters_opt:
    /*nothing*/ { [] }
  | parameters { $1 }

parameters:
  binding  { [$1] }
  | binding COMMA parameters { $1::$3 }

binding:
  ID COLON typ { ($3, $1) }
  | ID COLON_COLON typ { (List($3), $1) }

for_loop:
  FOR LPAREN expression COMMA assignment RPAREN COLON NEWLINE INDENT block DEDENT { For($3, $5, Block($10)) }
  | FOR ID IN expression COLON NEWLINE INDENT block DEDENT { ForIn($2, $4, Block($8)) }

while_loop:
  WHILE LPAREN expression RPAREN COLON NEWLINE INDENT block DEDENT { While($3, Block($8)) }

if_statement:
  IF LPAREN expression RPAREN COLON NEWLINE INDENT block DEDENT ELSE COLON NEWLINE INDENT block DEDENT { If($3, Block($8), Block($14)) }
  | IF LPAREN expression RPAREN COLON NEWLINE INDENT block DEDENT { If($3, Block($8), Block([])) }

/* Types */
typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | CHAR  { Char }
  | VOID   { Void }
  | typ LIST { List($1) }
  | LIST { List(Any) }

/* Lists */
list_literal:
  LBRACKET list_elements RBRACKET { ($2) }

 list_elements:
   expression { [$1] }
   | expression COMMA list_elements { $1 :: $3 }

/* Expressions */
assignment:
  | ID ASSIGN expression {Assign($1, $3)}
  | ID LBRACKET expression RBRACKET ASSIGN expression {AssignIndex($1, $3, $6)}
  | ID INCREMENT {Assign($1, Binop(Id($1), Plus, LitInt(1)))}
  | ID DECREMENT {Assign($1, Binop(Id($1), Minus, LitInt(1)))}

expression:
  | NULL { Null }
  | assignment { $1 }
  | ID LBRACKET expression RBRACKET { ListIndex($1, $3) }
  | INT_LIT { LitInt($1) }
  | BOOL_LIT { LitBool($1) }
  | CHAR_LIT { LitChar($1) }
  | FLOAT_LIT { LitFloat($1) }
  | ID          { Id($1) }
  | list_literal { LitList($1) }
  | NEW typ LBRACKET expression RBRACKET  { NewList($2, $4) }
  | expression PLUS expression { Binop($1, Plus,   $3) }
  | expression MINUS expression { Binop($1, Minus,   $3) }
  | expression TIMES expression { Binop($1, Times,   $3) }
  | expression DIVIDE expression { Binop($1, Divide,   $3) }
  | expression MOD expression { Binop( $1, Minus,   Binop(Binop($1, Divide, $3), Times, $3)) }
  | expression EQ expression { Binop($1, Eq,   $3) }
  | expression NEQ expression { Binop($1, Neq,   $3) }
  | expression LT expression { Binop($1, Lt,   $3) }
  | expression LEQ expression { Binop($1, Leq,   $3) }
  | expression GT expression { Binop($1, Gt,   $3) }
  | expression GEQ expression { Binop($1, Geq,   $3) }
  | expression AND expression { Binop($1, And,   $3) }
  | expression OR expression { Binop($1, Or,   $3) }
  | MINUS INT_LIT { LitInt($2 * -1)}
  | MINUS FLOAT_LIT { LitFloat(($2 *. (-1.0)))}
  | NOT expression { UnaryOp(Negate, $2) }
  | function_call { $1 }
  | LPAREN expression RPAREN { $2 } (*// For grouping and precedence*)

function_call:
  ID LPAREN arguments_opt RPAREN { Call($1, $3)}
  | ID DOT ID LPAREN arguments_opt RPAREN { Assign($1, Call($3, Id($1)::$5) )}

arguments_opt:
  /*nothing*/ { [] }
  | arguments { $1 }

arguments:
  expression  { [$1] }
  | expression COMMA arguments { $1::$3 }

