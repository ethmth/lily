/* Ocamlyacc parser for LILY */

// %{
// open Ast
// %}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN
%token EQ NEQ LT AND OR
%token IF ELSE WHILE INT LONG FLOAT BOOL
%token RETURN COMMA
%token <int> LITERAL
%token <float> FLIT
%token <bool> BLIT
%token <string> ID
%token EOF








// %start program
// %type <Ast.tokenseq> program
