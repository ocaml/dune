open Dune_rpc_lwt.V1.Action_plugin
module Glob = Dune_rpc_lwt.V1.Action_plugin.Glob
module Error = Dune_rpc_lwt.V1.Action_plugin.Error

let run action = Lwt_main.run (Dune_rpc_lwt.V1.Action_plugin.run action)

let%expect_test "absolute paths" =
  run (fun dap ->
    let open Lwt.Syntax in
    let path = Filename.concat (Sys.getcwd ()) "some_dir/some_file" in
    let+ data = read_file dap ~path in
    print_endline data);
  [%expect {| Hello from foo! |}]
;;

let%expect_test _ =
  let action dap =
    let open Lwt.Syntax in
    let+ data = read_file dap ~path:"some_dir/some_file" in
    print_endline data
  in
  run action;
  [%expect
    {|
    Hello from foo!
  |}]
;;

let%expect_test _ =
  let action dap =
    let open Lwt.Syntax in
    let+ data = read_directory_with_glob dap ~glob:Glob.universal ~path:"some_dir" in
    String.concat "," data |> print_endline
  in
  run action;
  [%expect
    {|
    some_file,subdir
  |}]
;;

let%expect_test "standalone reads use the current directory" =
  let cwd = Sys.getcwd () in
  Fun.protect
    ~finally:(fun () -> Sys.chdir cwd)
    (fun () ->
       let dap = outside_of_dune in
       Sys.chdir "some_dir";
       let open Lwt.Syntax in
       Lwt_main.run
         (let* contents = read_file dap ~path:"some_file" in
          print_endline contents;
          let+ entries = read_directory_with_glob dap ~path:"." ~glob:Glob.universal in
          print_endline (String.concat "," entries)));
  [%expect
    {|
    Hello from foo!
    some_file,subdir
    |}]
;;

let%expect_test "run captures synchronous failures" =
  (try
     let promise =
       Dune_rpc_lwt.V1.Action_plugin.run (fun _ -> failwith "callback failed")
     in
     print_endline "promise returned";
     Lwt_main.run promise
   with
   | Failure message -> print_endline message);
  [%expect
    {|
    promise returned
    callback failed
    |}]
;;

let run_action_expect_throws action =
  try
    run action;
    print_endline "SHOULD BE UNREACHABLE"
  with
  | Error.E message -> print_endline message
;;

let%expect_test _ =
  let action dap =
    let open Lwt.Syntax in
    let+ data = read_file dap ~path:"file_that_does_not_exist" in
    ignore data
  in
  run_action_expect_throws action;
  [%expect {| read_file: open(file_that_does_not_exist): No such file or directory |}]
;;

let%expect_test _ =
  let action dap =
    let open Lwt.Syntax in
    let+ entries =
      read_directory_with_glob
        dap
        ~glob:Glob.universal
        ~path:"directory_that_does_not_exist"
    in
    Printf.printf "[%s]\n" (String.concat ";" entries)
  in
  run action;
  [%expect {| [] |}]
;;

let%expect_test "reading a file as a directory gives an empty listing" =
  let action dap =
    let open Lwt.Syntax in
    let+ entries =
      read_directory_with_glob dap ~glob:Glob.universal ~path:"some_dir/some_file"
    in
    Printf.printf "[%s]\n" (String.concat ";" entries)
  in
  run action;
  [%expect {| [] |}]
;;
