open Stdune
open Dune_rules

let () = Dune_tests_common.init ()

let expand args =
  let { Action_builder.With_targets.build; targets = _ } =
    Command.expand ~dir:Path.root args
  in
  let result, _deps =
    Fiber.run
      (Memo.run (Action_builder.evaluate_and_collect_deps build))
      ~iter:(fun () -> assert false)
  in
  Appendable_list.to_list result
;;

let%expect_test "nested argument groups preserve order" =
  let open Command.Args in
  let args =
    S
      [ A "a"
      ; S []
      ; S [ As [ "b"; "c" ]; S [ A "d"; S []; S [ As [ "e"; "f" ] ] ] ]
      ; A "g"
      ]
  in
  List.iter (expand args) ~f:(printfn "%S");
  [%expect
    {|
    "a"
    "b"
    "c"
    "d"
    "e"
    "f"
    "g"
    |}]
;;
