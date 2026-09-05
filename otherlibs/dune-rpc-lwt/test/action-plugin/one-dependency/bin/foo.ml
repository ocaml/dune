open Dune_rpc_lwt.V1.Action_plugin

let ordinary_action dap =
  let open Lwt.Syntax in
  let* data = read_file dap ~path:"some_dependency" in
  Lwt_io.printl data
;;

let replace_file path contents =
  Sys.remove path;
  let output = open_out path in
  output_string output contents;
  close_out output
;;

let sandbox_action dap =
  let open Lwt.Syntax in
  print_endline "starting sandboxed action";
  if not (List.mem ".sandbox" (String.split_on_char '/' (Sys.getcwd ())))
  then failwith "expected a sandbox";
  if Sys.file_exists "../choice" then failwith "undeclared dependency is visible";
  replace_file "../inputs/static.txt" "local";
  let* choice, repeated =
    Lwt.both (read_file dap ~path:"../choice") (read_file dap ~path:"../choice")
  in
  if choice <> repeated then failwith "inconsistent dependency contents";
  let* data = read_file dap ~path:choice in
  let* listing =
    read_directory_with_glob dap ~path:"../inputs" ~glob:(Glob.of_string "*.txt")
  in
  let* () =
    let+ repeated = read_file dap ~path:choice in
    if data <> repeated then failwith "inconsistent dependency contents"
  in
  let* static = read_file dap ~path:"../inputs/static.txt" in
  let* () =
    let+ empty =
      read_directory_with_glob dap ~path:"../empty" ~glob:(Glob.of_string "*")
    in
    if empty <> [] || not (Sys.is_directory "../empty")
    then failwith "missing empty dependency directory"
  in
  if Sys.file_exists "../inputs/unused" then failwith "unmatched dependency is visible";
  let* (_ : string) = read_file dap ~path:"../tree/sub/first" in
  replace_file "../tree/sub/first" "local tree";
  let* () =
    let+ tree = read_directory_with_glob dap ~path:".." ~glob:(Glob.of_string "tree") in
    if tree <> [ "tree" ] then failwith "missing directory target"
  in
  let* first = read_file dap ~path:"../tree/sub/first" in
  let* second = read_file dap ~path:"../tree/sub/second" in
  if first <> "local tree" || second <> "second"
  then failwith "incorrect overlapping directory contents";
  Lwt_io.with_file ~mode:Output "../result" (fun output ->
    Lwt_io.fprintf output "%s\n%s\n%s\n" data (String.concat ", " listing) static)
;;

let action dap =
  match Sys.argv with
  | [| _ |] -> ordinary_action dap
  | [| _; "sandbox" |] -> sandbox_action dap
  | _ -> invalid_arg "invalid arguments"
;;

let () = run action
