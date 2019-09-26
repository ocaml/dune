open Stdune
open Dune
open Action_unexpanded.Infer.Outcome
open Dune_tests_common

let () = init ()

let p = Path.of_string

let b = Path.Build.of_string

let pb x = Path.build (Path.Build.of_string x)

let infer (a : Action.t) =
  let open Dyn.Encoder in
  let x = Action_unexpanded.Infer.infer a in
  ( List.map (Path.Set.to_list x.deps) ~f:Path.to_string
  , List.map (Path.Build.Set.to_list x.targets) ~f:Path.Build.to_string )
  |> pair (list string) (list string)
  |> print_dyn

let%expect_test _ =
  infer (Copy (p "a", b "b"));
  [%expect {|
  ([ "a" ], [ "_build/b" ])
  |}]

let%expect_test _ =
  infer (Progn [ Copy (p "a", b "b"); Copy (pb "b", b "c") ]);
  [%expect {|
([ "a" ], [ "_build/b"; "_build/c" ])
|}]

let%expect_test _ =
  (* CR-someday jdimino: ideally "b" should be treated as a non-buildable
     targets. As long as [rename] is not available in the DSL given to user, we
     don't need to care about this too much. *)
  infer (Progn [ Copy (p "a", b "b"); Rename (b "b", b "c") ]);
  [%expect {|
([ "a" ], [ "_build/b"; "_build/c" ])
|}]
