module T4 = struct
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  let to_dyn f g h i (a, b, c, d) = Dyn.Tuple [ f a; g b; h c; i d ]
end

module Git_config_parser = struct
  let to_dyn = T4.to_dyn Dyn.string (Dyn.option Dyn.string) Dyn.string Dyn.string
end

let print_or_fail l =
  match Dune_pkg.Rev_store.At_rev.Config.parse l with
  | Some v -> print_endline @@ Dyn.to_string @@ Git_config_parser.to_dyn v
  | None -> Printf.eprintf "Failed to parse %S\n" l
;;

let%expect_test "parsing simple section" =
  let config = "foo.bar=baz" in
  print_or_fail config;
  [%expect {|
  ("foo", None, "bar", "baz")
  |}]
;;

let%expect_test "parsing with arguments" =
  let config = "foo.bar.baz=qux" in
  print_or_fail config;
  [%expect {|
  ("foo", Some "bar", "baz", "qux")
  |}]
;;

let%expect_test "parsing with dots in name" =
  let config = "branch.compat-5.0-dune-2.9.remote=origin" in
  print_or_fail config;
  [%expect {|
  ("branch", Some "compat-5.0-dune-2.9", "remote", "origin")
  |}]
;;
