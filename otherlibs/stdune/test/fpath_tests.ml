open Stdune

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
