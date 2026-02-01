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
