open Parserscanner
let tokenize (lexbuf: Lexing.lexbuf) =
  let stokens = Scanner.token lexbuf
in match stokens with
  | DEDENT -> INDENT
  | _ -> stokens