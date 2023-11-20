open Stdune
module Checksum = Dune_pkg.Checksum
module Lock_dir = Dune_pkg.Lock_dir
module Expanded_variable_bindings = Dune_pkg.Solver_stats.Expanded_variable_bindings
module Variable = Dune_pkg.Solver_env.Variable
module Package_version = Dune_pkg.Package_version
module Package_name = Dune_lang.Package_name

let () = Dune_tests_common.init ()

let lock_dir_encode_decode_round_trip_test ~lock_dir_path ~lock_dir =
  let lock_dir_path = Path.Source.of_string lock_dir_path in
  Lock_dir.Write_disk.(
    prepare ~lock_dir_path ~files:Package_name.Map.empty lock_dir |> commit);
  let lock_dir_round_tripped =
    try Lock_dir.read_disk lock_dir_path with
    | User_error.E _ as exn ->
      let metadata_path =
        Path.Source.relative lock_dir_path Lock_dir.metadata_filename |> Path.source
      in
      let metadata_file_contents = Io.read_file metadata_path in
      print_endline
        "Failed to parse lockdir. Dumping raw metadata file to assist debugging.";
      print_endline metadata_file_contents;
      Exn.raise exn
  in
  if Lock_dir.equal
       (Lock_dir.remove_locs lock_dir_round_tripped)
       (Lock_dir.remove_locs lock_dir)
  then print_endline "lockdir matches after roundtrip:"
  else print_endline "lockdir doesn't match after roundtrip:";
  print_endline (Lock_dir.to_dyn lock_dir_round_tripped |> Dyn.to_string)
;;

let%expect_test "encode/decode round trip test for lockdir with no deps" =
  lock_dir_encode_decode_round_trip_test
    ~lock_dir_path:"empty_lock_dir"
    ~lock_dir:
      (Lock_dir.create_latest_version
         Package_name.Map.empty
         ~local_packages:[]
         ~ocaml:None
         ~repos:None
         ~expanded_solver_variable_bindings:Expanded_variable_bindings.empty);
  [%expect
    {|
    lockdir matches after roundtrip:
    { version = (0, 1)
    ; dependency_hash = None
    ; packages = map {}
    ; ocaml = None
    ; repos = { complete = true; used = None }
    ; expanded_solver_variable_bindings =
        { variable_values = []; unset_variables = [] }
    } |}]
;;

let empty_package name ~version =
  { Lock_dir.Pkg.build_command = None
  ; install_command = None
  ; deps = []
  ; info =
      { Lock_dir.Pkg_info.name; version; dev = false; source = None; extra_sources = [] }
  ; exported_env = []
  }
;;

let%expect_test "encode/decode round trip test for lockdir with simple deps" =
  lock_dir_encode_decode_round_trip_test
    ~lock_dir_path:"simple_lock_dir"
    ~lock_dir:
      (let mk_pkg_basic ~name ~version =
         let name = Package_name.of_string name in
         name, empty_package name ~version
       in
       Lock_dir.create_latest_version
         ~local_packages:[]
         ~ocaml:(Some (Loc.none, Package_name.of_string "ocaml"))
         ~repos:None
         ~expanded_solver_variable_bindings:
           { Expanded_variable_bindings.variable_values = [ Variable.Sys `Os, "linux" ]
           ; unset_variables = [ Variable.Sys `Os_family ]
           }
         (Package_name.Map.of_list_exn
            [ mk_pkg_basic ~name:"foo" ~version:(Package_version.of_string "0.1.0")
            ; mk_pkg_basic ~name:"bar" ~version:(Package_version.of_string "0.2.0")
            ]));
  [%expect
    {|
    lockdir matches after roundtrip:
    { version = (0, 1)
    ; dependency_hash = None
    ; packages =
        map
          { "bar" :
              { build_command = None
              ; install_command = None
              ; deps = []
              ; info =
                  { name = "bar"
                  ; version = "0.2.0"
                  ; dev = false
                  ; source = None
                  ; extra_sources = []
                  }
              ; exported_env = []
              }
          ; "foo" :
              { build_command = None
              ; install_command = None
              ; deps = []
              ; info =
                  { name = "foo"
                  ; version = "0.1.0"
                  ; dev = false
                  ; source = None
                  ; extra_sources = []
                  }
              ; exported_env = []
              }
          }
    ; ocaml = Some ("simple_lock_dir/lock.dune:3", "ocaml")
    ; repos = { complete = true; used = None }
    ; expanded_solver_variable_bindings =
        { variable_values = [ (Sys "os", "linux") ]
        ; unset_variables = [ Sys "os-family" ]
        }
    } |}]
;;

let%expect_test "encode/decode round trip test for lockdir with complex deps" =
  let module Action = Dune_lang.Action in
  let module String_with_vars = Dune_lang.String_with_vars in
  lock_dir_encode_decode_round_trip_test
    ~lock_dir_path:"complex_lock_dir"
    ~lock_dir:
      (let pkg_a =
         let name = Package_name.of_string "a" in
         let extra_source : Lock_dir.Source.t =
           External_copy (Loc.none, Path.External.of_string "/tmp/a")
         in
         ( name
         , let pkg = empty_package name ~version:(Package_version.of_string "0.1.0") in
           { pkg with
             build_command =
               Some
                 Action.(Progn [ Echo [ String_with_vars.make_text Loc.none "hello" ] ])
           ; install_command =
               Some
                 (Action.System
                    (* String_with_vars.t doesn't round trip so we have to set
                       [quoted] if the string would be quoted *)
                    (String_with_vars.make_text ~quoted:true Loc.none "echo 'world'"))
           ; info =
               { pkg.info with
                 dev = false
               ; source = Some extra_source
               ; extra_sources =
                   [ Path.Local.of_string "one", extra_source
                   ; ( Path.Local.of_string "two"
                     , Fetch { url = Loc.none, "randomurl"; checksum = None } )
                   ]
               }
           ; exported_env =
               [ { Action.Env_update.op = Eq
                 ; var = "foo"
                 ; value = String_with_vars.make_text Loc.none "bar"
                 }
               ]
           } )
       in
       let pkg_b =
         let name = Package_name.of_string "b" in
         ( name
         , let pkg = empty_package name ~version:(Package_version.of_string "dev") in
           { pkg with
             install_command = None
           ; deps = [ Loc.none, fst pkg_a ]
           ; info =
               { pkg.info with
                 dev = true
               ; source =
                   Some
                     (Fetch
                        { url = Loc.none, "https://github.com/foo/b"
                        ; checksum =
                            Some
                              ( Loc.none
                              , Checksum.of_string
                                  "sha256=adfc38f14c0188a2ad80d61451d011d27ab8839b717492d7ad42f7cb911c54c3"
                              )
                        })
               }
           } )
       in
       let pkg_c =
         let name = Package_name.of_string "c" in
         ( name
         , let pkg = empty_package name ~version:(Package_version.of_string "0.2") in
           { pkg with
             deps = [ Loc.none, fst pkg_a; Loc.none, fst pkg_b ]
           ; info =
               { pkg.info with
                 dev = false
               ; source =
                   Some
                     (Fetch
                        { url = Loc.none, "https://github.com/foo/c"; checksum = None })
               }
           } )
       in
       let opam_repo =
         let repo_id = Some (Dune_pkg.Repository_id.of_git_hash "95cf548dc") in
         Dune_pkg.Opam_repo.Private.create ~source:(Some "well-known-repo") ~repo_id
       in
       Lock_dir.create_latest_version
         ~local_packages:[]
         ~ocaml:(Some (Loc.none, Package_name.of_string "ocaml"))
         ~repos:(Some [ opam_repo ])
         ~expanded_solver_variable_bindings:Expanded_variable_bindings.empty
         (Package_name.Map.of_list_exn [ pkg_a; pkg_b; pkg_c ]));
  [%expect
    {|
    lockdir matches after roundtrip:
    { version = (0, 1)
    ; dependency_hash = None
    ; packages =
        map
          { "a" :
              { build_command = Some [ "progn"; [ "echo"; "hello" ] ]
              ; install_command = Some [ "system"; "echo 'world'" ]
              ; deps = []
              ; info =
                  { name = "a"
                  ; version = "0.1.0"
                  ; dev = false
                  ; source = Some External_copy External "/tmp/a"
                  ; extra_sources =
                      [ ("one", External_copy External "/tmp/a")
                      ; ("two", Fetch "randomurl", None)
                      ]
                  }
              ; exported_env = [ { op = "="; var = "foo"; value = "bar" } ]
              }
          ; "b" :
              { build_command = None
              ; install_command = None
              ; deps = [ ("complex_lock_dir/b.pkg:3", "a") ]
              ; info =
                  { name = "b"
                  ; version = "dev"
                  ; dev = true
                  ; source =
                      Some
                        Fetch
                          "https://github.com/foo/b",
                          Some
                            "sha256=adfc38f14c0188a2ad80d61451d011d27ab8839b717492d7ad42f7cb911c54c3"
                  ; extra_sources = []
                  }
              ; exported_env = []
              }
          ; "c" :
              { build_command = None
              ; install_command = None
              ; deps =
                  [ ("complex_lock_dir/c.pkg:3", "a")
                  ; ("complex_lock_dir/c.pkg:3", "b")
                  ]
              ; info =
                  { name = "c"
                  ; version = "0.2"
                  ; dev = false
                  ; source = Some Fetch "https://github.com/foo/c", None
                  ; extra_sources = []
                  }
              ; exported_env = []
              }
          }
    ; ocaml = Some ("complex_lock_dir/lock.dune:3", "ocaml")
    ; repos =
        { complete = true
        ; used =
            Some
              [ opam_repo_serializable
                  Some Git_hash "95cf548dc",
                  "well-known-repo"
              ]
        }
    ; expanded_solver_variable_bindings =
        { variable_values = []; unset_variables = [] }
    } |}]
;;
