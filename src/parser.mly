/* Author(s): Michaela Gary */
/* Last Edited: April 1, 2024 */
/* Ocamllex parser for LILY */

%{
open Ast
%}

/* Seperators */
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET

/* Punctuation */
%token COLON COMMA DECIMAL ARROW

/* Binary Operators */
%token PLUS MINUS TIMES DIVIDE

/* Assignment Operators */
%token ASSIGN

/* Relational Operators */
%token EQ NEQ GT LT GEQ LEQ 

/* Functional Operators */
%token MAP FILTER REDUCE 

/* Logical Operators */
%token AND OR NOT

/* Control Flow */
%token IF IS IN NONE ELSE ELIF FOR TRY CATCH WHILE FINALLY BREAK RETURN CONTINUE

/* Declaration */
%token CONST DEF LET

/* Types */
%token BOOL INT FLOAT CHAR STRING

/* Literals */
%token <int> INT_LIT FLOAT_LIT
%token <bool> BOOL_LIT
%token <string> ID CHAR_LIT STRING_LIT

/* Miscellaneous */
%token EOF

%start program
%type <Ast.program> program

/* Operator Associativity */
%left PLUS MINUS
%left TIMES DIVIDE
%right ASSIGN
%left EQ NEQ
%left GT LT
%nonassoc GEQ LEQ
%nonassoc MAP FILTER
%left REDUCE
%left AND OR 
%right NOT

%%

/* The grammar rules below here are for LILY from the LRM Parser section. */

/* add function declarations*/
program:
  statements EOF { $1 }

statements:
  /* nothing */ { [] }
  | statement statements  { $1::$2 }

// TODO Allow for multiple ways of declaration
declaration: 
  // LET vdeclaration { $2 }
  // | vdeclaration { $1 }
  LET ID COLON type ASSIGN expression { $4, $2 }

// vdeclaration:
//   ID ASSIGN expression
//   | ID COLON type ASSIGN expression
//   | ID COLON COLON type ASSIGN expression

// TODO May need to change this to avoid ambiguity, allow for ELIFs
if_statement:
  IF LPAREN expression RPAREN statement
  | IF LPAREN expression RPAREN statement ELSE statement

while_loop:
  WHILE LPAREN expression RPAREN statement

for_loop:
  FOR ID IN expression statement

// TODO need to fix this function definition, may have ambiguity with no ending
function_def:
  DEF ID LPAREN parameters_opt RPAREN ARROW type COLON statements

parameters_opt:
    /*nothing*/ { [] }
  | parameters { $1 }

parameters:
  expression  { [$1] }
  | expression COMMA parameters { $1::$3 }


// TODO NEED TO FIX ALL THESE TRY STATEMENT THINGS, MAYBE LAST PRIORITY
try_statement:
  TRY COLON statements catch_clauses

catch_clauses:
  catch_clause

catch_clause:
  CATCH LPAREN ID type RPAREN COLON statements

finally_clause:
  FINALLY COLON statements

type:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | CHAR  { Char }
  | STRING { String }

statement:
    declaration { $1 }
  | if_statement { $1 }
  | while_loop  { $1 }
  | for_loop  { $1 }
  | function_def { $1 }
  | try_statement { $1 }
  | expression_statement { $1 }

expression_statement:
  expression { $1 }

expression:
  /* From LRM: expression ('+' | '-' | '*' | '/') expression */ 
    expression PLUS expression { Binop($1, Add,   $3) }
  | expression MINUS expression { Binop($1, Sub,   $3) }
  | expression TIMES expression { Binop($1, Mult,   $3) }
  | expression DIVIDE expression { Binop($1, Div,   $3) }
    /* From LRM: expression ('==' | '!=' | '<' | '<=' | '>' | '>=') expression */ 
  | expression EQ expression { Binop($1, Eq,   $3) }
  | expression NEQ expression { Binop($1, Neq,   $3) }
  | expression LT expression { Binop($1, Lt,   $3) }
  | expression LEQ expression { Binop($1, Leq,   $3) }
  | expression GT expression { Binop($1, Gt,   $3) }
  | expression GEQ expression { Binop($1, Geq,   $3) }
    /* From LRM: the rest */ 
  | function_call { $1 }
  | list_declaration { $1 }
  | LPAREN expression RPAREN { $1 } // For grouping and precedence

function_call:
  ID LPAREN arguments_opt RPAREN {$1, $3}

arguments_opt:
  /*nothing*/ { [] }
  | arguments { $1 }

arguments:
  expression  { [$1] }
  | expression COMMA arguments { $1::$3 }

// TODO: Fix List declaration to adjust for multiple forms.
list_declaration:
  LET ID COLON COLON type ASSIGN LBRACE elements_opt RBRACE

elements_opt:
  /*nothing*/ { [] }
  | elements { $1 }

elements:
  expression  { [$1] }
  | expression COMMA elements { $1::$3 }


/* The grammar rules below were not modified. They are from Micro C. */

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ ID { ($1, $2) }

typ:
    INT   { Int   }
  | BOOL  { Bool  }

/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr SEMI                        { Return $2      }

expr:
    LITERAL          { Literal($1)            }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
