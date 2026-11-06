open Stdune
module Runparam = Ocaml.Runparam

let () = Dune_tests_common.init ()

let add_defaults =
  Runparam.add_defaults
    ~minor_heap_size:(2 * 1024 * 1024)
    ~small_heap_limit:(Some 16777216)
;;

let%expect_test "compose runtime parameters in one override" =
  add_defaults ~inherited:Env.empty ~overrides:Env.empty
  |> Env.to_unix
  |> List.iter ~f:print_endline;
  Runparam.add_defaults
    ~minor_heap_size:(2 * 1024 * 1024)
    ~small_heap_limit:None
    ~inherited:Env.empty
    ~overrides:Env.empty
  |> Env.to_unix
  |> List.iter ~f:print_endline;
  [%expect
    {|
    OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216
    OCAMLRUNPARAM=s=2M
    |}]
;;

let%expect_test "only OCAMLRUNPARAM opts out, including empty settings" =
  List.iter [ "OCAMLRUNPARAM"; "CAMLRUNPARAM" ] ~f:(fun name ->
    let var = Env.Var.of_string name in
    List.iter [ "o=42"; "" ] ~f:(fun value ->
      let env = Env.add Env.empty ~var ~value in
      Printf.printf
        "%s=%S: override=%b inherited=%b\n"
        (Env.Var.to_string var)
        value
        (Env.equal env (add_defaults ~inherited:Env.empty ~overrides:env))
        (Env.is_empty (add_defaults ~inherited:env ~overrides:Env.empty))));
  [%expect
    {|
    OCAMLRUNPARAM="o=42": override=true inherited=true
    OCAMLRUNPARAM="": override=true inherited=true
    CAMLRUNPARAM="o=42": override=false inherited=false
    CAMLRUNPARAM="": override=false inherited=false
    |}]
;;

let%expect_test "inherited paths are not copied and existing overrides survive" =
  let path = Env.Var.of_string "MANPATH" in
  let inherited = Env.add Env.empty ~var:path ~value:"/one:/two" in
  let overrides = Env.add Env.empty ~var:(Env.Var.of_string "OTHER") ~value:"kept" in
  let env = add_defaults ~inherited ~overrides in
  Printf.printf "MANPATH copied: %b\n" (Env.mem env ~var:path);
  Env.to_unix env |> List.sort ~compare:String.compare |> List.iter ~f:print_endline;
  [%expect
    {|
    MANPATH copied: false
    OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216
    OTHER=kept
    |}]
;;
