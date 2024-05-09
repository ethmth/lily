(* Function to read all lines from a file into a single string *)
let read_file filename =
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true; do
      lines := !lines ^ (input_line chan) ^ "\n"
    done; !lines
  with
  | End_of_file ->
    close_in chan;
    !lines


let get_file_path:string =
  let executable_path = Sys.argv.(0) in
  let cwd = Sys.getcwd () in
  let full_path =
    if Filename.is_relative executable_path then
      Filename.concat cwd executable_path
    else
      executable_path
  in
  let directory = Filename.dirname full_path in
  let resource_path = Filename.concat directory "stdlib.lily" in
  (* Now use resource_path as the path to your file *)
  (* print_endline ("Resource path: " ^ resource_path); *)
  resource_path
  

let get_program_string (program_file: string option):string =
  (* let read_stdin:string =
    ignore(print_endline ("In read_stdin"));
  let rec loop acc =
    try
      let line = input_line stdin in
      loop (acc ^ line ^ "\n")  (* Append each line with a newline *)
    with
      End_of_file -> acc
  in
  loop "" in *)
  (* Read a string from standard input *)
  let file_contents: string =
    match program_file with
      None ->
        (* ignore(print_endline "Reading from stdin"); *)
        let rec loop acc =
          try
            let line = input_line stdin in
            loop (acc ^ line ^ "\n")  (* Append each line with a newline *)
          with
            End_of_file -> acc
        in
        loop ""
      (* "" *)
      | Some(filename) ->
      read_file filename
    in

  (* Read contents from a file *)
  let lib_filename = get_file_path in
  let lib_file_contents = read_file lib_filename in
  (* ignore(get_file_path); *)
  (* Concatenate the input with the file contents *)
  let combined = lib_file_contents ^ "\n" ^ file_contents in
  (* ignore(print_endline ("COMBINED\n" ^ combined)); *)
    combined
  (* Print the concatenated result *)
  (* print_string "Combined contents:\n"; *)
  (* print_endline combined *)