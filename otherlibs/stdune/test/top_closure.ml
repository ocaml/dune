open! Stdune

let deps_of_list list dep = List.assoc_opt dep list |> Option.value ~default:[]

let top list ~deps =
  let dyn =
    let res = Top_closure.String.top_closure list ~key:Fun.id ~deps:(deps_of_list deps) in
    let f = Dyn.(list string) in
    Result.to_dyn f f res
  in
  print_endline (Dyn.to_string dyn)
;;

let%expect_test "trivial" =
  top [ "entry" ] ~deps:[ "entry", [] ];
  [%expect {| Ok [ "entry" ] |}]
;;

let%expect_test "no cycle" =
  top [ "deps"; "entry" ] ~deps:[ "entry", []; "deps", [ "entry" ] ];
  [%expect {| Ok [ "entry"; "deps" ] |}]
;;

let%expect_test "cycle" =
  let top = top ~deps:[ "foo", [ "bar" ]; "bar", [ "foo" ] ] in
  top [ "foo" ];
  [%expect {| Error [ "foo"; "bar"; "foo" ] |}];
  top [ "bar" ];
  [%expect {| Error [ "bar"; "foo"; "bar" ] |}]
;;
