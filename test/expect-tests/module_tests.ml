open Stdune
module Kind = Dune_rules.Module.Kind

(* See #10264 *)
let%expect_test "Module.Kind encoding round trip" =
  let module_name s = Dune_rules.Module_name.of_string s in
  let test k =
    let ast = Kind.encode k in
    let sexp = Dune_sexp.Ast.add_loc ~loc:Loc.none ast in
    let decoded =
      match Dune_lang.Decoder.parse Kind.decode Univ_map.empty sexp with
      | r -> Ok r
      | exception e -> Error e
    in
    let dyn =
      Dyn.record
        [ "ast", Dyn.string (Dune_sexp.to_string ast)
        ; "decoded", Or_exn.to_dyn Kind.to_dyn decoded
        ]
    in
    Dune_tests_common.print_dyn dyn
  in
  test Impl;
  [%expect {| { ast = "impl"; decoded = Ok Impl } |}];
  test (Alias []);
  [%expect {| { ast = "alias"; decoded = Ok Alias [] } |}];
  test (Alias [ module_name "A" ]);
  [%expect {| { ast = "(alias (A))"; decoded = Ok Alias [ "A" ] } |}];
  test (Alias [ module_name "A"; module_name "B" ]);
  [%expect {| { ast = "(alias (A B))"; decoded = Ok Alias [ "A"; "B" ] } |}]
;;
