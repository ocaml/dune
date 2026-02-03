open Stdune

let%expect_test "making a directory for an existing file" =
  let dir = Temp.create Dir ~prefix:"fpath" ~suffix:"test" in
  let fn = Path.relative dir "foo" in
  Io.write_file fn "";
  (* This does not error, but it will if it ends with a "/" on MacOS *)
  ignore (Fpath.mkdir (Path.to_string fn));
  [%expect {| |}];
  Path.mkdir_p fn;
  (* This in turn does not error *)
  [%expect {| |}]
;;

let%expect_test "is_root" =
  [ ""; "."; "/"; "foo/bar"; "/foo" ]
  |> List.iter ~f:(fun path -> printfn "Fpath.is_root %S = %b" path (Fpath.is_root path));
  [%expect
    {|
    Fpath.is_root "" = false
    Fpath.is_root "." = true
    Fpath.is_root "/" = true
    Fpath.is_root "foo/bar" = false
    Fpath.is_root "/foo" = false |}]
;;

let%expect_test "mkdir_p" =
  let test path =
    match Fpath.mkdir_p path with
    | exception exn -> Exn.to_dyn exn |> Dyn.to_string |> print_endline
    | res ->
      Fpath.dyn_of_mkdir_p_result res |> Dyn.to_string |> print_endline;
      if not (Sys.is_directory path) then print_endline "failed to create directory"
  in
  test "foo";
  [%expect {| Created |}];
  test "foo/bar";
  [%expect {| Created |}];
  test "x/y/z";
  [%expect {| Created |}];
  test "x/y/z";
  [%expect {| Already_exists |}];
  Io.String_path.write_file "baz" "";
  test "baz";
  [%expect
    {|
    Already_exists
    failed to create directory
    |}];
  Unix.mkdir "dir" 0o777;
  Io.String_path.write_file "dir/baz" "";
  test "dir/baz";
  [%expect
    {|
    Already_exists
    failed to create directory
    |}]
;;
