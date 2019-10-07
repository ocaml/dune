open Dune
open Import
open Dune_tests_common

let () = init ()

let db_path =
  Path.of_filename_relative_to_initial_cwd "../unit-tests/findlib-db"

let print_pkg ppf pkg =
  let info = Dune_package.Lib.info pkg in
  let name = Lib_info.name info in
  Format.fprintf ppf "<package:%s>" (Lib_name.to_string name)

let findlib =
  let cwd = Path.of_filename_relative_to_initial_cwd (Sys.getcwd ()) in
  Findlib.create ~stdlib_dir:cwd ~paths:[ db_path ]
    ~version:(Ocaml_version.make (4, 02, 3))

let%expect_test _ =
  let pkg =
    match Findlib.find findlib (Lib_name.of_string_exn ~loc:None "foo") with
    | Ok (Library x) -> x
    | _ -> assert false
  in
  (* "foo" should depend on "baz" *)
  let info = Dune_package.Lib.info pkg in
  let requires = Lib_info.requires info in
  let dyn = Dyn.Encoder.list Lib_dep.to_dyn requires in
  let pp = Dyn.pp dyn in
  Format.printf "%a@." Pp.render_ignore_tags pp;
  [%expect {|[ "baz" ]|}]

(* Meta parsing/simplification *)

let%expect_test _ =
  Path.relative db_path "foo/META"
  |> Meta.load ~name:(Some (Lib_name.of_string_exn ~loc:None "foo"))
  |> Meta.Simplified.to_dyn |> print_dyn;
  [%expect
    {|
    { name = Some "foo"
    ;
    vars =
      map
        {
        "requires" :
          {
          set_rules =
            [ { var = "requires"; predicates = []; action = Set; value = "bar" }
            ;
            { var = "requires"
            ; predicates = [ Pos "ppx_driver" ]
            ; action = Set
            ; value = "baz"
            }
            ]
          ; add_rules = []
          }
        }
    ; subs = []
    } |}]

let conf =
  Findlib.Config.load
    (Path.relative db_path "../toolchain")
    ~toolchain:"tlc" ~context:"<context>"

let%expect_test _ =
  print_dyn (Findlib.Config.to_dyn conf);
  [%expect
    {|
    {
    vars =
      map
        {
        "FOO_BAR" :
          {
          set_rules =
            [
            { preds_required = set { 6; 7 }
            ; preds_forbidden = set {}
            ; value = "my variable"
            }
            ]
          ; add_rules = []
          }
        }
    ; preds = set { 6 }
    } |}];
  print_dyn (Env.to_dyn (Findlib.Config.env conf));
  [%expect {| map { "FOO_BAR" : "my variable" } |}]
