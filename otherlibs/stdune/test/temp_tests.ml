open Stdune
open Dune_tests_common
open Dyn

let () = init ()

let%expect_test "Temp.clear_dir works" =
  let path = Temp.create Dir ~prefix:"dune" ~suffix:"unit_test" in
  Io.write_file (Path.relative path "foo") "";
  let print () =
    Path.readdir_unsorted path |> Result.to_dyn (list string) opaque |> print_dyn
  in
  print ();
  Temp.clear_dir path;
  print ();
  [%expect {|
    Ok [ "foo" ]
    Ok []
  |}]
;;
