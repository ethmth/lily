/* Author(s): Michaela Gary */
/* Last Edited: April 1, 2024 */
/* Ocamllex parser for LILY */

%{
open Astscanner
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
%type <Astscanner.program> program

%%

/* The grammar rules below here are for LILY from the LRM Parser section. */

/* add function declarations*/
program:
  tokens EOF { $1 }

tokens:
  /* nothing */ { [] }
  | token tokens  { $1::$2 }

token:
   /* New Line */
  NEWLINE     { StringToken("NEWLINE") }
  /* Seperators */
  | LPAREN    { StringToken("LPAREN")}
  | RPAREN    { StringToken("RPAREN")}
  | LBRACE    { StringToken("LBRACE")}
  | RBRACE    { StringToken("RBRACE")}
  | LBRACKET   { StringToken("LBRACKET")}
  | RBRACKET    { StringToken("RBRACKET")}
  /* Punctuation */
  | COLON     { StringToken("COLON")}
  | COMMA     { StringToken("COMMA")}
  | ARROW     { StringToken("ARROW")}
  | DOT       { StringToken("DOT")}
  /* Binary Operators */
  | PLUS      { StringToken("PLUS")}
  | MINUS     { StringToken("MINUS")}
  | TIMES     { StringToken("TIMES")}
  | DIVIDE    { StringToken("DIVIDE")}
  /* Assignment Operators */
  | ASSIGN    { StringToken("ASSIGN")}
  /* Relational Operators */
  | EQ        { StringToken("EQ")}
  | NEQ       { StringToken("NEQ")}
  | GT        { StringToken("GT")}
  | LT        { StringToken("LT")}
  | GEQ       { StringToken("GEQ")}
  | LEQ       { StringToken("LEQ")}
  /* Functional Operators */
  | MAP       { StringToken("MAP")}
  | FILTER    { StringToken("FILTER")}
  | REDUCE    { StringToken("REDUCE")}
  /* Logical Operators */
  | AND       { StringToken("AND")}
  | OR        { StringToken("OR")}
  | NOT       { StringToken("NOT")}
  /* Control Flow */
  | IF        { StringToken("IF")}
  | IS        { StringToken("IS")}
  | IN        { StringToken("IN")}
  | NONE      { StringToken("NONE")}
  | ELSE      { StringToken("ELSE")}
  | ELIF      { StringToken("ELIF")}
  | FOR       { StringToken("FOR")}
  | TRY       { StringToken("TRY")}
  | CATCH      { StringToken("CATCH")}
  | WHILE     { StringToken("WHILE")}
  | FINALLY     { StringToken("FINALLY")}
  | BREAK     { StringToken("BREAK")}
  | RETURN    { StringToken("RETURN")}
  | CONTINUE    { StringToken("CONTINUE")}
  /* Declaration */
  | CONST     { StringToken("CONST")}
  | DEF       { StringToken("DEF")}
  | LET       { StringToken("LET")}
  /* Types */
  | BOOL      { StringToken("BOOL")}
  | INT       { StringToken("INT")}
  | FLOAT      { StringToken("FLOAT")}
  | CHAR      { StringToken("CHAR")}
  | STRING    { StringToken("STRING")}
  /* Literals */
  | INT_LIT       { StringAndValueToken("INT_LIT", ValInt($1))}
  | FLOAT_LIT     { StringAndValueToken("FLOAT_LIT", ValFloat($1))}
  | BOOL_LIT      { StringAndValueToken("BOOL_LIT", ValBool($1))}
  | CHAR_LIT      { StringAndValueToken("CHAR_LIT", ValChar($1))}
  | STRING_LIT    { StringAndValueToken("STRING_LIT", ValString($1))}
  | ID            { StringAndValueToken("ID", ValString($1))}
  /* Indentation Tokens */
  | DEDENT      { StringToken("DEDENT")}
  | INDENT    { StringToken("INDENT")}
  | NEWLINE     { StringAndValueToken("NEWLINEI", ValInt($1)) }