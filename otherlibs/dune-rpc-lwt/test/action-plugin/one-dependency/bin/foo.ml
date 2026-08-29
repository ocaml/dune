open Dune_rpc_lwt.V1.Action_plugin

let ordinary_action dap ~path =
  let open Lwt.Syntax in
  let* data = read_file dap ~path in
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

let detached_action dap state =
  let open Lwt.Syntax in
  let* () =
    Lwt_io.with_file ~mode:Output (Filename.concat state "pid") (fun output ->
      Lwt_io.fprintf output "%d" (Unix.getpid ()))
  in
  let rec wait_started () =
    if Sys.file_exists (Filename.concat state "started")
    then Lwt.return_unit
    else
      let* () = Lwt_unix.sleep 0.01 in
      wait_started ()
  in
  let* () =
    Lwt.choose [ Lwt.map ignore (read_file dap ~path:"slow-input"); wait_started () ]
  in
  let* () =
    Lwt_io.with_file ~mode:Output "detached" (fun output -> Lwt_io.write output "done")
  in
  Lwt_io.printl "ran"
;;

let write_connection path =
  let temp = path ^ ".tmp" in
  let output = open_out temp in
  output_string output (Sys.getenv "DUNE_DYNAMIC_RUN_ACTION_ID");
  output_char output '\n';
  output_string output (Sys.getenv "DUNE_RPC");
  output_char output '\n';
  close_out output;
  Sys.rename temp path
;;

let held_action _dap ~connection ~release =
  let open Lwt.Syntax in
  let* () =
    Lwt_io.with_file ~mode:Output "held-target" (fun output -> Lwt_io.write output "held")
  in
  write_connection connection;
  let rec loop () =
    if Sys.file_exists release
    then Lwt.return_unit
    else
      let* () = Lwt_unix.sleep 0.05 in
      loop ()
  in
  loop ()
;;

let action dap =
  match Sys.argv with
  | [| _ |] -> ordinary_action dap ~path:"some_dependency"
  | [| _; "read"; path |] -> ordinary_action dap ~path
  | [| _; "sandbox" |] -> sandbox_action dap
  | [| _; "detached"; state |] -> detached_action dap state
  | [| _; "hold"; connection; release |] -> held_action dap ~connection ~release
  | [| _; "initialize" |] -> Lwt.return_unit
  | _ -> invalid_arg "invalid arguments"
;;

let () = run action
