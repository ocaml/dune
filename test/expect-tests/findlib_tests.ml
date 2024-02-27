open Stdune
module Lib_name = Dune_lang.Lib_name
module Meta = Dune_findlib.Findlib.Meta
module Findlib_config = Dune_findlib.Findlib.Config
module Lib_dep = Dune_lang.Lib_dep
open Dune_rules
open Dune_rules.For_tests
open Dune_tests_common

let () = init ()

let foo_meta = {|
requires = "bar"
requires(ppx_driver) = "baz"
|}

let db_path : Path.Outside_build_dir.t =
  External (Path.External.of_filename_relative_to_initial_cwd "../unit-tests/findlib-db")
;;

let print_pkg ppf pkg =
  let info = Dune_package.Lib.info pkg in
  let name = Lib_info.name info in
  Format.fprintf ppf "<package:%s>" (Lib_name.to_string name)
;;

let findlib =
  let lib_config : Lib_config.t =
    { has_native = true
    ; ext_lib = ".a"
    ; ext_obj = ".o"
    ; os_type = Ocaml_config.Os_type.Other ""
    ; architecture = ""
    ; system = ""
    ; model = ""
    ; natdynlink_supported = Dynlink_supported.By_the_os.of_bool true
    ; ext_dll = ".so"
    ; stdlib_dir = Path.source @@ Path.Source.(relative root) "stdlib"
    ; ccomp_type = Other "gcc"
    ; ocaml_version_string = "4.02.3"
    ; ocaml_version = Ocaml.Version.make (4, 14, 1)
    }
  in
  Memo.lazy_ (fun () ->
    Findlib.For_tests.create ~paths:[ Path.outside_build_dir db_path ] ~lib_config)
;;

let resolve_pkg s =
  (let lib_name = Lib_name.of_string s in
   let open Memo.O in
   let* findlib = Memo.Lazy.force findlib in
   Findlib.find findlib lib_name)
  |> Memo.run
  |> Test_scheduler.(run (create ()))
;;

let elide_db_path path =
  let prefix = Path.Outside_build_dir.to_string db_path in
  let path = Path.to_string path in
  String.drop_prefix_if_exists path ~prefix
;;

let print_pkg_archives pkg =
  let pkg = resolve_pkg pkg in
  let print_lib kind entry =
    let entry =
      Dune_package.Lib.info entry
      |> Lib_info.archives
      |> Ocaml.Mode.Dict.map ~f:(List.map ~f:elide_db_path)
      |> Ocaml.Mode.Dict.to_dyn (Dyn.list Dyn.string)
    in
    Dyn.variant
      (match kind with
       | `Available -> "Available"
       | `Hidden -> "Hidden")
      [ entry ]
    |> print_dyn
  in
  match pkg with
  | Ok (Library x) -> print_lib `Available x
  | Ok (Hidden_library x) -> print_lib `Hidden x
  | Ok e -> Dune_package.Entry.to_dyn e |> print_dyn
  | Error err -> Findlib.Unavailable_reason.to_dyn err |> print_dyn
;;

let%expect_test _ =
  print_pkg_archives "qux";
  [%expect {| Available { byte = [ "/qux/qux.cma" ]; native = [] } |}]
;;

let%expect_test _ =
  print_pkg_archives "xyz";
  [%expect {| Available { byte = [ "/xyz.cma" ]; native = [] } |}]
;;

let () = Printexc.record_backtrace true

let%expect_test "configurator" =
  print_pkg_archives "dune.configurator";
  [%expect
    {|
    Deprecated_library_name
      { old_public_name = "dune.configurator"
      ; new_public_name = "dune-configurator"
      } |}]
;;

let%expect_test "builtins" =
  print_pkg_archives "str";
  [%expect {|
    Available { byte = []; native = [] } |}];
  print_pkg_archives "dynlink";
  [%expect
    {|
    Hidden
      { byte = [ "stdlib/dynlink.cma" ]; native = [ "stdlib/dynlink.cmxa" ] } |}]
;;

let%expect_test _ =
  let pkg =
    match resolve_pkg "foo" with
    | Ok (Library x) -> x
    | _ -> assert false
  in
  (* "foo" should depend on "baz" *)
  let info = Dune_package.Lib.info pkg in
  let requires = Lib_info.requires info in
  let dyn = Dyn.list Lib_dep.to_dyn requires in
  let pp = Dyn.pp dyn in
  Format.printf "%a@." Pp.to_fmt pp;
  [%expect {|[ "baz" ]|}]
;;

(* Meta parsing/simplification *)

let%expect_test _ =
  Meta.of_string foo_meta ~name:(Some (Package.Name.of_string "foo"))
  |> Meta.Simplified.to_dyn
  |> print_dyn;
  [%expect
    {|
    { name = Some "foo"
    ; vars =
        map
          { "requires" :
              { set_rules =
                  [ { var = "requires"
                    ; predicates = []
                    ; action = Set
                    ; value = "bar"
                    }
                  ; { var = "requires"
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
;;

let conf () =
  let memo =
    let open Memo.O in
    Findlib_config.discover_from_env
      ~which:(fun _ -> assert false)
      ~ocamlpath:(Memo.return [])
      ~env:
        (Env.initial
         |> Env.add
              ~var:"OCAMLFIND_CONF"
              ~value:
                (Path.Outside_build_dir.relative db_path "../toolchain"
                 |> Path.Outside_build_dir.to_string))
      ~findlib_toolchain:(Some "tlc")
    >>| Option.value_exn
  in
  Memo.run memo |> Test_scheduler.(run (create ()))
;;

let%expect_test _ =
  let conf = conf () in
  print_dyn (Findlib_config.to_dyn conf);
  [%expect
    {|
    { config =
        { vars =
            map
              { "FOO_BAR" :
                  { set_rules =
                      [ { preds_required = set { "env"; "tlc" }
                        ; preds_forbidden = set {}
                        ; value = "my variable"
                        }
                      ]
                  ; add_rules = []
                  }
              }
        ; preds = set { "tlc" }
        }
    ; toolchain = Some "tlc"
    } |}];
  print_dyn (Env.to_dyn (Findlib_config.env conf));
  [%expect {| map { "FOO_BAR" : "my variable" } |}];
  Findlib_config.ocamlpath conf
  |> Memo.run
  |> Test_scheduler.(run (create ()))
  |> Dyn.(list Path.to_dyn)
  |> print_dyn;
  [%expect {| [] |}]
;;
