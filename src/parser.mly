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
