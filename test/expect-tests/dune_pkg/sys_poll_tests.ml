open Import

let parse lines =
  Dune_pkg.Sys_poll.parse_os_release lines
  |> List.iter ~f:(fun (key, value) -> Printf.printf "%s: %s\n" key value)
;;

let%expect_test "parsing os-release values" =
  parse [ "ID=debian"; "ID_LIKE=\"rhel\""; "VERSION_ID='2.18'"; "NAME=" ];
  [%expect
    {|
    ID: debian
    ID_LIKE: rhel
    VERSION_ID: 2.18
    NAME:
    |}]
;;
