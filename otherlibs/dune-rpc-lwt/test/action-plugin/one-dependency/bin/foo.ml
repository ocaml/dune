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

let detached_action dap state ~cancel =
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
  let pending = read_file dap ~path:"slow-input" in
  let* () = Lwt.choose [ Lwt.map ignore pending; wait_started () ] in
  if cancel then Lwt.cancel pending;
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

let absolute_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
;;

let change_dir dir =
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
  Sys.chdir dir
;;

let action dap =
  match Sys.argv with
  | [| _; "read-in"; dir; path |] ->
    change_dir dir;
    ordinary_action dap ~path
  | [| _; "list-in"; dir |] ->
    change_dir dir;
    let open Lwt.Syntax in
    let* files = read_directory_with_glob dap ~path:"." ~glob:(Glob.of_string "*.txt") in
    Lwt_list.iter_s Lwt_io.printl files
  | [| _; "in-flight" |] ->
    let pending = read_file dap ~path:"input" in
    change_dir "elsewhere";
    let open Lwt.Syntax in
    let* contents = pending in
    Lwt_io.printl contents
  | [| _; "list-in-flight" |] ->
    let pending = read_directory_with_glob dap ~path:"." ~glob:(Glob.of_string "*.txt") in
    change_dir "elsewhere";
    let open Lwt.Syntax in
    let* files = pending in
    Lwt_list.iter_s Lwt_io.printl files
  | [| _; "helper-in" |] ->
    let prog = Filename.concat (Sys.getcwd ()) "foo.exe" in
    let open Lwt.Syntax in
    Lwt_process.with_process_in
      ~cwd:"elsewhere"
      (prog, [| prog; "read"; "input" |])
      (fun process ->
         let* contents = Lwt_io.read process#stdout in
         let* status = process#status in
         match status with
         | Unix.WEXITED 0 -> Lwt_io.print contents
         | _ -> failwith "helper failed")
  | [| _; "batch"; first; second |] ->
    let open Lwt.Syntax in
    let first_path = absolute_path first in
    let second_path = absolute_path second in
    let* () = build_deps dap [ Dep.File first; Dep.File second; Dep.File first ] in
    Lwt_process.exec ("cat", [| "cat"; first_path; second_path |])
    |> Lwt.map (function
      | Unix.WEXITED 0 -> ()
      | _ -> failwith "cat failed")
  | [| _; "mixed-batch" |] ->
    change_dir "mixed";
    let open Lwt.Syntax in
    let paths = List.map absolute_path [ "one"; "glob/two.txt"; "directory/three" ] in
    let pending =
      build_deps
        dap
        [ Dep.File "one"
        ; Dep.Glob { path = "glob"; glob = "*.txt" }
        ; Dep.Directory "directory"
        ; Dep.File "one"
        ]
    in
    change_dir "..";
    let* () = pending in
    Lwt_process.exec ("cat", Array.of_list ("cat" :: paths))
    |> Lwt.map (function
      | Unix.WEXITED 0 -> ()
      | _ -> failwith "cat failed")
  | [| _; "absolute"; path |] ->
    let open Lwt.Syntax in
    let* data = read_file dap ~path:(absolute_path path) in
    Lwt_io.printl data
  | [| _ |] -> ordinary_action dap ~path:"some_dependency"
  | [| _; "read"; path |] -> ordinary_action dap ~path
  | [| _; "sandbox" |] -> sandbox_action dap
  | [| _; "detached"; state |] -> detached_action dap state ~cancel:false
  | [| _; "cancelled"; state |] -> detached_action dap state ~cancel:true
  | [| _; "hold"; connection; release |] -> held_action dap ~connection ~release
  | [| _; "initialize" |] -> Lwt.return_unit
  | [| _; "exit"; code |] -> exit (int_of_string code)
  | _ -> invalid_arg "invalid arguments"
;;

let () =
  try
    Lwt_main.run
      (match Sys.argv with
       | [| _; "promise" |] ->
         let open Lwt.Syntax in
         let* () = run (fun dap -> ordinary_action dap ~path:"some_dependency") in
         Lwt_io.printl "returned"
       | _ -> run action)
  with
  | Error.E message ->
    prerr_endline message;
    exit 1
;;
