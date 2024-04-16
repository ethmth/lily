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

open Parsing;;
let _ = parse_error;;
# 6 "parser1.mly"
open Ast
# 68 "parser1.ml"
let yytransl_const = [|
  257 (* NEWLINE *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* COLON *);
  265 (* COMMA *);
  266 (* ARROW *);
  267 (* DOT *);
  268 (* PLUS *);
  269 (* MINUS *);
  270 (* TIMES *);
  271 (* DIVIDE *);
  272 (* ASSIGN *);
  273 (* EQ *);
  274 (* NEQ *);
  275 (* GT *);
  276 (* LT *);
  277 (* GEQ *);
  278 (* LEQ *);
  279 (* MAP *);
  280 (* FILTER *);
  281 (* REDUCE *);
  282 (* AND *);
  283 (* OR *);
  284 (* NOT *);
  285 (* IF *);
  286 (* IS *);
  287 (* IN *);
  288 (* NONE *);
  289 (* ELSE *);
  290 (* ELIF *);
  291 (* FOR *);
  292 (* TRY *);
  293 (* EXCEPT *);
  294 (* WHILE *);
  295 (* FINALLY *);
  296 (* BREAK *);
  297 (* RETURN *);
  298 (* CONTINUE *);
  299 (* CONST *);
  300 (* DEF *);
  301 (* LET *);
  302 (* BOOL *);
  303 (* INT *);
  304 (* FLOAT *);
  305 (* CHAR *);
  306 (* STRING *);
    0 (* EOF *);
  314 (* INDENT *);
  315 (* DEDENT *);
    0|]

let yytransl_block = [|
  307 (* INT_LIT *);
  308 (* FLOAT_LIT *);
  309 (* BOOL_LIT *);
  310 (* CHAR_LIT *);
  311 (* STRING_LIT *);
  312 (* ID *);
  313 (* NEWLINEI *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\004\000\
\004\000\005\000\005\000\005\000\005\000\005\000\005\000\006\000\
\006\000\006\000\006\000\007\000\009\000\008\000\010\000\018\000\
\018\000\019\000\019\000\020\000\011\000\012\000\013\000\014\000\
\015\000\016\000\016\000\016\000\016\000\016\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\021\000\022\000\022\000\023\000\023\000\000\000"

let yylen = "\002\000\
\003\000\000\000\002\000\002\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\006\000\
\004\000\004\000\002\000\003\000\002\000\001\000\012\000\000\000\
\001\000\001\000\003\000\003\000\011\000\009\000\009\000\009\000\
\006\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\001\000\003\000\
\008\000\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\063\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\039\000\042\000\040\000\
\041\000\043\000\000\000\000\000\000\000\000\000\005\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\015\000\000\000\055\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\003\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\060\000\
\000\000\000\000\000\000\047\000\048\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\025\000\000\000\035\000\034\000\036\000\
\037\000\038\000\000\000\000\000\000\000\058\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\062\000\000\000\000\000\033\000\000\000\000\000\000\000\028\000\
\000\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\057\000\000\000\000\000\000\000\000\000\000\000\031\000\
\032\000\000\000\030\000\000\000\000\000\000\000\029\000\000\000\
\023\000"

let yydgoto = "\002\000\
\004\000\020\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\099\000\
\034\000\091\000\092\000\093\000\035\000\071\000\072\000"

let yysindex = "\003\000\
\033\255\000\000\038\255\000\000\007\255\034\255\037\255\040\255\
\045\255\062\255\007\255\012\255\013\255\000\000\000\000\000\000\
\000\000\000\000\030\255\070\000\038\255\073\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\075\000\000\000\075\255\219\255\007\255\074\255\007\255\
\007\255\007\255\075\000\076\255\025\255\007\255\007\255\000\000\
\000\000\000\000\007\255\007\255\007\255\007\255\007\255\007\255\
\007\255\007\255\007\255\007\255\069\255\232\255\023\255\245\255\
\050\000\002\000\028\255\053\255\007\255\064\000\083\255\000\000\
\075\000\248\254\248\254\000\000\000\000\191\255\191\255\122\255\
\122\255\122\255\122\255\031\255\080\255\038\255\087\255\007\255\
\088\255\089\255\103\255\000\000\099\255\000\000\000\000\000\000\
\000\000\000\000\094\255\075\000\007\255\000\000\113\255\112\255\
\057\255\117\255\015\000\118\255\053\255\132\255\028\255\007\255\
\000\000\007\255\064\255\000\000\092\255\144\255\095\255\000\000\
\053\255\000\000\075\000\151\255\038\255\038\255\154\255\038\255\
\150\255\000\000\097\255\100\255\104\255\105\255\162\255\000\000\
\000\000\038\255\000\000\114\255\115\255\038\255\000\000\116\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\176\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\126\255\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\176\255\000\000\002\255\000\000\000\000\000\000\000\000\
\000\000\000\000\180\255\000\000\183\255\182\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\111\255\000\000\000\000\000\000\
\000\000\000\000\190\255\000\000\000\000\198\255\000\000\000\000\
\213\255\148\255\170\255\000\000\000\000\037\000\040\000\009\255\
\177\255\199\255\206\255\000\000\000\000\171\255\000\000\000\000\
\000\000\000\000\000\000\000\000\226\255\000\000\000\000\000\000\
\000\000\000\000\241\255\242\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\182\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\254\255\000\000\171\255\171\255\000\000\171\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\171\255\000\000\000\000\000\000\171\255\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\235\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\160\255\
\253\255\000\000\145\000\000\000\000\000\154\000\168\000"

let yytablesize = 353
let yytable = "\049\000\
\002\000\037\000\044\000\001\000\044\000\053\000\054\000\043\000\
\005\000\053\000\044\000\053\000\120\000\044\000\044\000\044\000\
\044\000\053\000\044\000\044\000\044\000\044\000\044\000\044\000\
\129\000\053\000\053\000\053\000\053\000\053\000\053\000\046\000\
\068\000\003\000\062\000\038\000\064\000\065\000\066\000\005\000\
\069\000\040\000\070\000\073\000\039\000\047\000\041\000\074\000\
\075\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\083\000\014\000\015\000\016\000\017\000\018\000\036\000\042\000\
\105\000\100\000\006\000\044\000\045\000\048\000\007\000\008\000\
\009\000\050\000\063\000\010\000\046\000\067\000\011\000\084\000\
\086\000\012\000\013\000\090\000\107\000\102\000\103\000\104\000\
\014\000\015\000\016\000\017\000\018\000\019\000\106\000\108\000\
\109\000\070\000\094\000\095\000\096\000\097\000\098\000\131\000\
\132\000\110\000\134\000\111\000\123\000\112\000\070\000\056\000\
\115\000\056\000\114\000\116\000\141\000\117\000\119\000\056\000\
\144\000\125\000\056\000\056\000\056\000\056\000\044\000\056\000\
\056\000\056\000\056\000\056\000\056\000\051\000\052\000\053\000\
\054\000\044\000\044\000\044\000\044\000\121\000\044\000\044\000\
\044\000\044\000\044\000\044\000\045\000\126\000\045\000\127\000\
\128\000\130\000\133\000\136\000\045\000\135\000\137\000\045\000\
\045\000\138\000\140\000\139\000\045\000\045\000\045\000\045\000\
\045\000\045\000\046\000\142\000\046\000\143\000\145\000\002\000\
\022\000\051\000\046\000\051\000\021\000\046\000\046\000\019\000\
\059\000\051\000\046\000\046\000\046\000\046\000\046\000\046\000\
\024\000\051\000\051\000\051\000\051\000\051\000\051\000\054\000\
\061\000\054\000\051\000\052\000\053\000\054\000\052\000\054\000\
\052\000\057\000\058\000\059\000\060\000\020\000\052\000\054\000\
\054\000\054\000\054\000\054\000\054\000\061\000\052\000\052\000\
\052\000\052\000\052\000\052\000\026\000\002\000\051\000\052\000\
\053\000\054\000\085\000\055\000\056\000\057\000\058\000\059\000\
\060\000\017\000\018\000\051\000\052\000\053\000\054\000\087\000\
\055\000\056\000\057\000\058\000\059\000\060\000\016\000\122\000\
\051\000\052\000\053\000\054\000\089\000\055\000\056\000\057\000\
\058\000\059\000\060\000\124\000\113\000\051\000\052\000\053\000\
\054\000\118\000\055\000\056\000\057\000\058\000\059\000\060\000\
\000\000\000\000\051\000\052\000\053\000\054\000\000\000\055\000\
\056\000\057\000\058\000\059\000\060\000\049\000\000\000\049\000\
\050\000\000\000\050\000\000\000\000\000\049\000\000\000\000\000\
\050\000\000\000\000\000\000\000\000\000\049\000\049\000\000\000\
\050\000\050\000\088\000\002\000\000\000\051\000\052\000\053\000\
\054\000\000\000\055\000\056\000\057\000\058\000\059\000\060\000\
\101\000\000\000\000\000\051\000\052\000\053\000\054\000\000\000\
\055\000\056\000\057\000\058\000\059\000\060\000\051\000\052\000\
\053\000\054\000\000\000\055\000\056\000\057\000\058\000\059\000\
\060\000"

let yycheck = "\021\000\
\000\000\005\000\001\001\001\000\003\001\014\001\015\001\011\000\
\002\001\001\001\009\001\003\001\109\000\012\001\013\001\014\001\
\015\001\009\001\017\001\018\001\019\001\020\001\021\001\022\001\
\121\000\017\001\018\001\019\001\020\001\021\001\022\001\002\001\
\008\001\001\001\038\000\002\001\040\000\041\000\042\000\002\001\
\016\001\002\001\046\000\047\000\008\001\016\001\002\001\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\051\001\052\001\053\001\054\001\055\001\056\001\002\001\
\086\000\069\000\029\001\056\001\056\001\000\000\033\001\034\001\
\035\001\001\001\001\001\038\001\002\001\002\001\041\001\011\001\
\058\001\044\001\045\001\056\001\088\000\003\001\056\001\008\001\
\051\001\052\001\053\001\054\001\055\001\056\001\008\001\008\001\
\008\001\101\000\046\001\047\001\048\001\049\001\050\001\125\000\
\126\000\003\001\128\000\009\001\112\000\016\001\114\000\001\001\
\001\001\003\001\002\001\059\001\138\000\001\001\001\001\009\001\
\142\000\058\001\012\001\013\001\014\001\015\001\001\001\017\001\
\018\001\019\001\020\001\021\001\022\001\012\001\013\001\014\001\
\015\001\012\001\013\001\014\001\015\001\010\001\017\001\018\001\
\019\001\020\001\021\001\022\001\001\001\058\001\003\001\008\001\
\058\001\003\001\001\001\059\001\009\001\008\001\059\001\012\001\
\013\001\058\001\001\001\059\001\017\001\018\001\019\001\020\001\
\021\001\022\001\001\001\058\001\003\001\059\001\059\001\000\000\
\001\001\001\001\009\001\003\001\001\001\012\001\013\001\001\001\
\003\001\009\001\017\001\018\001\019\001\020\001\021\001\022\001\
\003\001\017\001\018\001\019\001\020\001\021\001\022\001\001\001\
\003\001\003\001\012\001\013\001\014\001\015\001\001\001\009\001\
\003\001\019\001\020\001\021\001\022\001\001\001\009\001\017\001\
\018\001\019\001\020\001\021\001\022\001\003\001\017\001\018\001\
\019\001\020\001\021\001\022\001\003\001\059\001\012\001\013\001\
\014\001\015\001\003\001\017\001\018\001\019\001\020\001\021\001\
\022\001\001\001\001\001\012\001\013\001\014\001\015\001\003\001\
\017\001\018\001\019\001\020\001\021\001\022\001\001\001\111\000\
\012\001\013\001\014\001\015\001\003\001\017\001\018\001\019\001\
\020\001\021\001\022\001\114\000\101\000\012\001\013\001\014\001\
\015\001\003\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\255\255\012\001\013\001\014\001\015\001\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\001\001\255\255\003\001\
\001\001\255\255\003\001\255\255\255\255\009\001\255\255\255\255\
\009\001\255\255\255\255\255\255\255\255\017\001\018\001\255\255\
\017\001\018\001\009\001\059\001\255\255\012\001\013\001\014\001\
\015\001\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\009\001\255\255\255\255\012\001\013\001\014\001\015\001\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\012\001\013\001\
\014\001\015\001\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001"

let yynames_const = "\
  NEWLINE\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  COLON\000\
  COMMA\000\
  ARROW\000\
  DOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  GT\000\
  LT\000\
  GEQ\000\
  LEQ\000\
  MAP\000\
  FILTER\000\
  REDUCE\000\
  AND\000\
  OR\000\
  NOT\000\
  IF\000\
  IS\000\
  IN\000\
  NONE\000\
  ELSE\000\
  ELIF\000\
  FOR\000\
  TRY\000\
  EXCEPT\000\
  WHILE\000\
  FINALLY\000\
  BREAK\000\
  RETURN\000\
  CONTINUE\000\
  CONST\000\
  DEF\000\
  LET\000\
  BOOL\000\
  INT\000\
  FLOAT\000\
  CHAR\000\
  STRING\000\
  EOF\000\
  INDENT\000\
  DEDENT\000\
  "

let yynames_block = "\
  INT_LIT\000\
  FLOAT_LIT\000\
  BOOL_LIT\000\
  CHAR_LIT\000\
  STRING_LIT\000\
  ID\000\
  NEWLINEI\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 78 "parser1.mly"
                         ( _2 )
# 396 "parser1.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser1.mly"
                ( [] )
# 402 "parser1.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 82 "parser1.mly"
                          ( _1::_2 )
# 410 "parser1.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_simple) in
    Obj.repr(
# 85 "parser1.mly"
                        ( _1 )
# 417 "parser1.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_compound) in
    Obj.repr(
# 86 "parser1.mly"
                  ( _1 )
# 424 "parser1.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 89 "parser1.mly"
                ( _1 )
# 431 "parser1.ml"
               : 'stmt_simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignment) in
    Obj.repr(
# 90 "parser1.mly"
               ( _1 )
# 438 "parser1.ml"
               : 'stmt_simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_statement) in
    Obj.repr(
# 91 "parser1.mly"
                         ( _1 )
# 445 "parser1.ml"
               : 'stmt_simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'return_statement) in
    Obj.repr(
# 92 "parser1.mly"
                     ( _1 )
# 452 "parser1.ml"
               : 'stmt_simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_def) in
    Obj.repr(
# 97 "parser1.mly"
                 ( _1 )
# 459 "parser1.ml"
               : 'stmt_compound))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'for_loop) in
    Obj.repr(
# 98 "parser1.mly"
              ( _1 )
# 466 "parser1.ml"
               : 'stmt_compound))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'while_loop) in
    Obj.repr(
# 99 "parser1.mly"
                ( _1 )
# 473 "parser1.ml"
               : 'stmt_compound))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'if_statement) in
    Obj.repr(
# 100 "parser1.mly"
                 ( _1 )
# 480 "parser1.ml"
               : 'stmt_compound))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'elif_statement) in
    Obj.repr(
# 101 "parser1.mly"
                   ( _1 )
# 487 "parser1.ml"
               : 'stmt_compound))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'else_statement) in
    Obj.repr(
# 102 "parser1.mly"
                   ( _1 )
# 494 "parser1.ml"
               : 'stmt_compound))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 109 "parser1.mly"
                                     ( DeclAssign(_4, _2, _6))
# 503 "parser1.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 110 "parser1.mly"
                     ( Decl(_4, _2) )
# 511 "parser1.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 111 "parser1.mly"
                             ( IDeclAssign(_2, _4) )
# 519 "parser1.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "parser1.mly"
           ( IDecl(_2) )
# 526 "parser1.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 115 "parser1.mly"
                         (Assign(_1, _3))
# 534 "parser1.ml"
               : 'assignment))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 118 "parser1.mly"
                      ( Return(_2) )
# 541 "parser1.ml"
               : 'return_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 121 "parser1.mly"
             ( ExprStmt(_1) )
# 548 "parser1.ml"
               : 'expression_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 8 : 'parameters_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 5 : 'typ) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 127 "parser1.mly"
  (
    Fdecl({
      rtyp=_7;
      fname=_2;
      parameters=_4;
      stmts=_11
    })
  )
# 565 "parser1.ml"
               : 'function_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser1.mly"
                ( [] )
# 571 "parser1.ml"
               : 'parameters_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parameters) in
    Obj.repr(
# 138 "parser1.mly"
               ( _1 )
# 578 "parser1.ml"
               : 'parameters_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binding) in
    Obj.repr(
# 141 "parser1.mly"
           ( [_1] )
# 585 "parser1.ml"
               : 'parameters))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binding) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parameters) in
    Obj.repr(
# 142 "parser1.mly"
                             ( _1::_3 )
# 593 "parser1.ml"
               : 'parameters))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 145 "parser1.mly"
               ( (_3, _1) )
# 601 "parser1.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'expression) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 148 "parser1.mly"
                                                                                       ( For(_3, _5, _10) )
# 610 "parser1.ml"
               : 'for_loop))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expression) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 151 "parser1.mly"
                                                                        ( While(_3, _8) )
# 618 "parser1.ml"
               : 'while_loop))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expression) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 154 "parser1.mly"
                                                                     ( If(_3, _8) )
# 626 "parser1.ml"
               : 'if_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expression) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 157 "parser1.mly"
                                                                       ( Elif(_3, _8) )
# 634 "parser1.ml"
               : 'elif_statement))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 160 "parser1.mly"
                                              ( Else(_5) )
# 641 "parser1.ml"
               : 'else_statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 175 "parser1.mly"
          ( Int   )
# 647 "parser1.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 176 "parser1.mly"
          ( Bool  )
# 653 "parser1.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "parser1.mly"
          ( Float )
# 659 "parser1.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 178 "parser1.mly"
          ( Char )
# 665 "parser1.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 179 "parser1.mly"
           ( String )
# 671 "parser1.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 197 "parser1.mly"
          ( LitInt(_1) )
# 678 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 198 "parser1.mly"
             ( LitBool(_1) )
# 685 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 199 "parser1.mly"
             ( LitChar(_1) )
# 692 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 200 "parser1.mly"
              ( LitFloat(_1) )
# 699 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 201 "parser1.mly"
               ( LitString(_1) )
# 706 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 202 "parser1.mly"
                ( Id(_1) )
# 713 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 203 "parser1.mly"
                               ( Binop(_1, Plus,   _3) )
# 721 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 204 "parser1.mly"
                                ( Binop(_1, Minus,   _3) )
# 729 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 205 "parser1.mly"
                                ( Binop(_1, Times,   _3) )
# 737 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 206 "parser1.mly"
                                 ( Binop(_1, Divide,   _3) )
# 745 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 207 "parser1.mly"
                             ( Binop(_1, Eq,   _3) )
# 753 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 208 "parser1.mly"
                              ( Binop(_1, Neq,   _3) )
# 761 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 209 "parser1.mly"
                             ( Binop(_1, Lt,   _3) )
# 769 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 210 "parser1.mly"
                              ( Binop(_1, Leq,   _3) )
# 777 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 211 "parser1.mly"
                             ( Binop(_1, Gt,   _3) )
# 785 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 212 "parser1.mly"
                              ( Binop(_1, Geq,   _3) )
# 793 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_call) in
    Obj.repr(
# 213 "parser1.mly"
                  ( _1 )
# 800 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 215 "parser1.mly"
                             ( _2 )
# 807 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'arguments_opt) in
    Obj.repr(
# 216 "parser1.mly"
                                                                ( MethodCall(_2, _5, _7) )
# 816 "parser1.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments_opt) in
    Obj.repr(
# 220 "parser1.mly"
                                 ( Call(_1, _3))
# 824 "parser1.ml"
               : 'function_call))
; (fun __caml_parser_env ->
    Obj.repr(
# 223 "parser1.mly"
              ( [] )
# 830 "parser1.ml"
               : 'arguments_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arguments) in
    Obj.repr(
# 224 "parser1.mly"
              ( _1 )
# 837 "parser1.ml"
               : 'arguments_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 227 "parser1.mly"
              ( [_1] )
# 844 "parser1.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments) in
    Obj.repr(
# 228 "parser1.mly"
                               ( _1::_3 )
# 852 "parser1.ml"
               : 'arguments))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
