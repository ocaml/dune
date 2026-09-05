open Dune_rpc_lwt.V1.Action_plugin
module Glob = Dune_rpc_lwt.V1.Action_plugin.Glob
module Error = Dune_rpc_lwt.V1.Action_plugin.Error

let run action = Lwt_main.run (action outside_of_dune)

let%expect_test _ =
  try run (fun dap -> read_file dap ~path:"/some/absolute/path" |> Lwt.map ignore) with
  | Invalid_argument message ->
    print_endline message;
    [%expect
      {| Path "/some/absolute/path" is absolute. All paths used with Dune_rpc.V1.Action_plugin must be relative. |}]
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
