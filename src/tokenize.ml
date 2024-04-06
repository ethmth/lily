open Parserscanner


let print_queue (queue) =
  let rec print_queue_helper (q) =
    match q with
    | h::t -> print_string (string_of_int h); print_queue_helper t
    | [] -> print_endline "" in
  print_queue_helper !queue

let process_newline (x: int) (queue) =
  NEWLINE

let tokenize =
  let queue = ref [] in
  fun (lexbuf: Lexing.lexbuf) ->
    match !queue with
    | h::t -> queue := t; h
    | [] ->
      let stokens = Scanner.token lexbuf
      in match stokens with
      | NEWLINEI(x) -> process_newline x queue
      | INDENT -> process_newline 5 queue
      | _ -> stokens