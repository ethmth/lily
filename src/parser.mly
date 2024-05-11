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
%token COLON COMMA ARROW
%token DOT

/* Binary Operators */
%token PLUS MINUS TIMES DIVIDE MOD
%token ELWISE_ADD  // Adding the new operator as a token (CHIMA)

/* Assignment Operators */
%token ASSIGN
%token INCREMENT DECREMENT

/* Relational Operators */
%token EQ NEQ GT LT GEQ LEQ 

/* Functional Operators */
%token MAP FILTER REDUCE 

/* Logical Operators */
%token AND OR NOT

/* Control Flow */
%token IF IS IN NONE ELSE ELIF FOR TRY EXCEPT WHILE FINALLY BREAK RETURN CONTINUE

/* Declaration */
%token CONST DEF LET

/* Types */
%token BOOL INT FLOAT CHAR STRING VOID

/* List */
%token LIST
%token EMPTY_LIST
%token COLON_COLON

/* Literals */
%token <int> INT_LIT 
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <string> ID


/* Additional functional operator for REDUCE */
%token WITH


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
%nonassoc MAP FILTER 
%nonassoc WITH
%nonassoc REDUCE
%nonassoc ELWISE_ADD


%%

/* The grammar rules below here are for LILY from the LRM Parser section. */

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
  // | BREAK
  // | CONTINUE

stmt_compound:
  function_def { $1 }
  | for_loop  { $1 }
  | while_loop  { $1 }
  | if_statement { $1 }
  // | elif_statement { $1 }
  // | else_statement { $1 }
  // | try_statement { $1 }

/* stmt_simple */
declaration: 
  /* regular declarations */
  LET ID COLON typ ASSIGN expression { DeclAssign($4, $2, $6)}
  | LET ID COLON typ { Decl($4, $2) }
  /* list declarations */
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

while_loop:
  WHILE LPAREN expression RPAREN COLON NEWLINE INDENT block DEDENT { While($3, Block($8)) }

if_statement:
  IF LPAREN expression RPAREN COLON NEWLINE INDENT block DEDENT ELSE COLON NEWLINE INDENT block DEDENT { If($3, Block($8), Block($14)) }
  | IF LPAREN expression RPAREN COLON NEWLINE INDENT block DEDENT { If($3, Block($8), Block([])) }

// elif_statement:
//   ELIF LPAREN expression RPAREN COLON NEWLINE INDENT block DEDENT { Elif($3, $8) }

// else_statement:
//   ELSE COLON NEWLINE INDENT statements DEDENT { Else($5) }

// try_statement:
//   | TRY COLON NEWLINE INDENT statements except_clauses finally_clause DEDENT
//     {
//       Try($5, $6, Some($7))
//     }
//   | TRY COLON NEWLINE INDENT statements except_clauses DEDENT
//     {
//       Try($5, $6, None)
//     }
// except_clauses:
//   | except_clause except_clauses { $1 :: $2 }
//   | /* nothing */ { [] }

// except_clause:
//   EXCEPT LPAREN typ ID RPAREN COLON NEWLINE INDENT statements DEDENT
//   {
//     { exn_type=Some $3; exn_var=Some $4; handler=$9 }
//   }
//   | EXCEPT COLON NEWLINE INDENT statements DEDENT
//   {
//     { exn_type=None; exn_var=None; handler=$5 }
//   }
// finally_clause:
//   FINALLY COLON NEWLINE INDENT statements DEDENT { $5 }

/* Types */
typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | CHAR  { Char }
  // | STRING { String }
  | VOID   { Void }
  | typ LIST { List($1) }

/* Lists */

list_literal:
  LBRACKET list_elements_opt RBRACKET { ($2) }

list_elements_opt:
   /* nothing */ { [] }
   | list_elements { $1 }

 list_elements:
   expression { [$1] }
   | expression COMMA list_elements { $1 :: $3 }


/* Expressions */

assignment:
  | ID ASSIGN expression {Assign($1, $3)}
  | ID INCREMENT {Assign($1, Binop(Id($1), Plus, LitInt(1)))}
  | ID DECREMENT {Assign($1, Binop(Id($1), Minus, LitInt(1)))}

expression:
  | assignment { $1 }
  // TODO: Make this work with non-Int literals
  | ID LBRACKET expression RBRACKET { ListIndex($1, $3) }
  | INT_LIT { LitInt($1) }
  | BOOL_LIT { LitBool($1) }
  | CHAR_LIT { LitChar($1) }
  | FLOAT_LIT { LitFloat($1) }
  // | STRING_LIT { LitString($1) }
  | ID          { Id($1) }
  | list_literal { LitList($1) }                   (*// Added this line to handle list literals as expressions (CHIMA)*)
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
  | NOT expression { UnaryOp(Negate, $2) }
  // | expression MAP expression { Map($1, $3) }      
  // | expression FILTER expression { Filter($1, $3) }   
  // | expression REDUCE expression WITH expression { Reduce($1, $3, $5) }
  // | expression ELWISE_ADD expression { ListBinop($1, ElwiseAdd, $3) }  (*// New element-wise addition (CHIMA)*)
  | function_call { $1 }
  | LPAREN expression RPAREN { $2 } (*// For grouping and precedence*)


function_call:
  ID LPAREN arguments_opt RPAREN { Call($1, $3)}
  | ID DOT ID LPAREN arguments_opt RPAREN { Assign($1, Call($3, Id($1)::$5) )}
  // | expression DOT ID LPAREN arguments_opt RPAREN { Call($3, $1::$5) }

arguments_opt:
  /*nothing*/ { [] }
  | arguments { $1 }

arguments:
  expression  { [$1] }
  | expression COMMA arguments { $1::$3 }

