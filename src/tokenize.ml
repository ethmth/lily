open Parser
(* UNCOMMENT THIS FOR test0: *)
(* open Parserscanner *)

let print_queue (queue) =
  let rec print_queue_helper (q) =
    match q with
    | h::t -> print_string (string_of_int h); print_queue_helper t
    | [] -> print_endline "" in
  print_queue_helper !queue

let rec queue_add_indent (x:int) (queue) =
    if x > 1 then raise (Failure("Tokenize Failure: Cannot indent more than once at a time"))
    else
    match x with 
    | 0 -> NEWLINE
    | _ -> queue := !queue @ [INDENT]; queue_add_indent (x - 1) queue

let rec queue_add_dedent (x:int) (queue) =
  match x with 
  | 0 -> NEWLINE
  | _ -> queue := !queue @ [DEDENT]; queue_add_dedent (x - 1) queue

let process_newline (this_indent: int) (queue) (curr_indent)=
  let curr_indent_old = !curr_indent in
  curr_indent := this_indent;
  if this_indent = curr_indent_old then
    NEWLINE
  else if this_indent < curr_indent_old then
    queue_add_dedent (curr_indent_old - this_indent) queue
  else
    queue_add_indent (this_indent - curr_indent_old) queue

let tokenize =
  let queue = ref [] in
  let curr_indent = ref 0 in
  fun (lexbuf: Lexing.lexbuf) ->
    match !queue with
    | h::t -> queue := t; h
    | [] ->
      let stokens = Scanner.token lexbuf
      in match stokens with
      | NEWLINEI(x) -> process_newline x queue curr_indent
      | _ -> stokens