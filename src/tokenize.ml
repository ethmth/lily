open Parser

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

let process_eof (queue) (curr_indent) (last_token) =
  if ((queue_add_dedent (!curr_indent) queue) = NEWLINE) then
    queue := !queue @ [EOF];
    if (!last_token = NEWLINE) then
      match !queue with
      | h::t -> queue := t; h
      | [] -> EOF
    else
      NEWLINE

let tokenize =
  let queue = ref [] in
  let curr_indent = ref 0 in
  let first_line = ref true in
  let last_token = ref NEWLINE in
  fun (lexbuf: Lexing.lexbuf) ->
    match !queue with
    | h::t -> queue := t; h
    | [] ->
      let stokens = Scanner.token lexbuf in 
      match stokens with
      | NEWLINEI(x) -> last_token := 
            (if !first_line then 
              (first_line := false; NEWLINE) else
            process_newline x queue curr_indent); !last_token
      | EOF -> process_eof queue curr_indent last_token
      | _ -> last_token := 
            (if !first_line then 
              (first_line := false;
               queue := !queue @ [stokens]; 
               NEWLINE) 
            else stokens); !last_token