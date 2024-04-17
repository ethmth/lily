/* Author(s): Michaela Gary, Ethan Thomas */
/* Last Edited: April 1, 2024 */
/* Ocamllex parser for LILY */

%{
open Ast
%}

/* New Line */
%token NEWLINE

/* Seperators */
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET


/* Punctuation */
%token COLON COMMA ARROW
%token DOT

/* Binary Operators */
%token PLUS MINUS TIMES DIVIDE
%token ELWISE_ADD  // Adding the new operator as a token (CHIMA)

/* Assignment Operators */
%token ASSIGN

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
%token BOOL INT FLOAT CHAR STRING

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
%left TIMES DIVIDE
%right NOT
%nonassoc MAP FILTER 
%nonassoc WITH
%nonassoc REDUCE
%nonassoc ELWISE_ADD


%%

/* The grammar rules below here are for LILY from the LRM Parser section. */

program:
  NEWLINE statements EOF { $2 }

statements:
  /* nothing */ { [] }
  | statement statements  { $1::$2 }

statement:
    stmt_simple NEWLINE { $1 }  // one-line statements
  | stmt_compound { $1 }        // multi-line "block" statements
stmt_simple:
    declaration { $1 }
  | assignment { $1 }
  | expression_statement { $1 }
  | return_statement { $1 }
  // | BREAK
  // | CONTINUE

stmt_compound:
  function_def { $1 }
  | for_loop  { $1 }
  | while_loop  { $1 }
  | if_statement { $1 }
  | elif_statement { $1 }
  | else_statement { $1 }
  // | try_statement { $1 }

/* stmt_simple */

// TODO (Ethan) Implement declaration: Allow for multiple ways of declaration
declaration: 
  LET ID COLON typ ASSIGN expression { DeclAssign($4, $2, $6)}
  | LET ID COLON typ { Decl($4, $2) }
  | LET ID ASSIGN expression { IDeclAssign($2, $4) }
  | LET ID { IDecl($2) }

assignment:
  | ID ASSIGN expression {Assign($1, $3)}

return_statement:
  | RETURN expression { Return($2) }

expression_statement:
  expression { ExprStmt($1) }

/* stmt_compound */

function_def:
  DEF ID LPAREN parameters_opt RPAREN ARROW typ COLON NEWLINE INDENT statements DEDENT
  {
    Fdecl({
      rtyp=$7;
      fname=$2;
      parameters=$4;
      stmts=$11
    })
  }

parameters_opt:
    /*nothing*/ { [] }
  | parameters { $1 }

parameters:
  binding  { [$1] }
  | binding COMMA parameters { $1::$3 }

binding:
  ID COLON typ { ($3, $1) }

for_loop:
  FOR LPAREN expression COMMA expression RPAREN COLON NEWLINE INDENT statements DEDENT { For($3, $5, $10) }

while_loop:
  WHILE LPAREN expression RPAREN COLON NEWLINE INDENT statements DEDENT { While($3, $8) }

if_statement:
  IF LPAREN expression RPAREN COLON NEWLINE INDENT statements DEDENT { If($3, $8) }

elif_statement:
  ELIF LPAREN expression RPAREN COLON NEWLINE INDENT statements DEDENT { Elif($3, $8) }

else_statement:
  ELSE COLON NEWLINE INDENT statements DEDENT { Else($5) }

// TODO (Tani) Implement try statements
// try_statement:
//   TRY COLON statements catch_clauses

// except_clause:
//   CATCH LPAREN ID type RPAREN COLON statements

// finally_clause:
//   FINALLY COLON statements

/* Types */

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | CHAR  { Char }
  | STRING { String }
  | LIST typ { List($2) }


/* Lists */

list_elements_opt:
   /* nothing */ { [] }
   | list_elements { $1 }

 list_elements:
   expression { [$1] }
   | expression COMMA list_elements { $1 :: $3 }

// TODO: (Chima) Implement Lists
// list_literal:
//    LBRACKET list_elements_opt RBRACKET { ListLit($2) }


/* Expressions */

expression:
  INT_LIT { LitInt($1) }
  | BOOL_LIT { LitBool($1) }
  | CHAR_LIT { LitChar($1) }
  | FLOAT_LIT { LitFloat($1) }
  | STRING_LIT { LitString($1) }
  | ID          { Id($1) }
  | expression PLUS expression { Binop($1, Plus,   $3) }
  | expression MINUS expression { Binop($1, Minus,   $3) }
  | expression TIMES expression { Binop($1, Times,   $3) }
  | expression DIVIDE expression { Binop($1, Divide,   $3) }
  | expression EQ expression { Binop($1, Eq,   $3) }
  | expression NEQ expression { Binop($1, Neq,   $3) }
  | expression LT expression { Binop($1, Lt,   $3) }
  | expression LEQ expression { Binop($1, Leq,   $3) }
  | expression GT expression { Binop($1, Gt,   $3) }
  | expression GEQ expression { Binop($1, Geq,   $3) }
  | expression MAP expression { Map($1, $3) }      
  | expression FILTER expression { Filter($1, $3) }   
  | expression REDUCE expression WITH expression { Reduce($1, $3, $5) }
  | expression ELWISE_ADD expression { ListBinop($1, ElwiseAdd, $3) }  (*// New element-wise addition (CHIMA)*)
  | function_call { $1 }
  | list_declaration { $1 }
  | list_literal { $1 }                   (*// Added this line to handle list literals as expressions (CHIMA)*)
  // | LET ID COLON typ ASSIGN expression { DeclExpr($4, $2, $6) }  (*// Adding new expression type for inline declarations*)
  | LPAREN expression RPAREN { $2 } (*// For grouping and precedence*)
  | LPAREN expression RPAREN DOT ID LPAREN arguments_opt RPAREN { MethodCall($2, $5, $7) }


function_call:
  ID LPAREN arguments_opt RPAREN { Call($1, $3)}

arguments_opt:
  /*nothing*/ { [] }
  | arguments { $1 }

arguments:
  expression  { [$1] }
  | expression COMMA arguments { $1::$3 }


// (Chima) Implement Lists (COMPLETED)
   list_literal:
      LBRACKET list_elements_opt RBRACKET { ListLit($2) }

// (Chima) Implement List Declaration (COMPLETED)
list_declaration:
  | LET ID COLON_COLON typ ASSIGN LBRACKET elements_opt RBRACKET { ListInit($2, $4, $7) }
  //| LET ID COLON_COLON typ EMPTY_LIST '=' empty_list { ListInit($2, $4, []) }
  // | LET ID COLON_COLON typ '=' LBRACKET elements_opt RBRACKET { ListInit($2, $4, $8) }  // Added rule for list init w/o empty list
  //|  LET ID COLON typ ASSIGN expression { DeclAssign($2, $4, $6) }  // Existing rule for simple types

 elements_opt:
   /*nothing*/ { [] }
   | elements { $1 }

 elements:
   expression  { [$1] }
   | expression COMMA elements { $1::$3 }

