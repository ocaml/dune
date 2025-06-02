open Stdune
module Checksum = Dune_pkg.Checksum
module Lock_dir = Dune_pkg.Lock_dir
module Dependency = Dune_pkg.Lock_dir.Dependency
module Opam_repo = Dune_pkg.Opam_repo
module Expanded_variable_bindings = Dune_pkg.Solver_stats.Expanded_variable_bindings
module Package_variable_name = Dune_lang.Package_variable_name
module Variable_value = Dune_pkg.Variable_value
module Rev_store = Dune_pkg.Rev_store
module Package_version = Dune_pkg.Package_version
module Source = Dune_pkg.Source
module Package_name = Dune_lang.Package_name
module Scheduler = Dune_engine.Scheduler

let () = Dune_tests_common.init ()

module Update = struct
  open Dyn

  let update_source ~commit = function
    | String url as v ->
      let opam_url = OpamUrl.parse url in
      (match Option.equal String.equal opam_url.hash (Some commit) with
       | false -> v
       | true ->
         let opam_url = { opam_url with hash = Some "MATCHES_EXPECTED" } in
         String (OpamUrl.to_string opam_url))
    | otherwise -> otherwise
  ;;

  let update_used ~commit = function
    | Option (Some (List xs)) ->
      let xs =
        List.map xs ~f:(function
          | Variant (("opam_repo_serializable" as u), [ source ]) ->
            let source = update_source ~commit source in
            Variant (u, [ source ])
          | otherwise -> otherwise)
      in
      Option (Some (List xs))
    | otherwise -> otherwise
  ;;

  let update_repositories ~commit = function
    | Record xs ->
      let xs =
        List.map xs ~f:(function
          | ("used" as u), dyn -> u, update_used ~commit dyn
          | otherwise -> otherwise)
      in
      Record xs
    | otherwise -> otherwise
  ;;

  let update_lock_dir_dyn ~commit = function
    | Record xs ->
      let xs =
        List.map xs ~f:(function
          | ("repos" as u), dyn -> u, update_repositories ~commit dyn
          | otherwise -> otherwise)
      in
      Record xs
    | otherwise -> otherwise
  ;;
end

let lock_dir_encode_decode_round_trip_test ?commit ~lock_dir_path ~lock_dir () =
  let lock_dir_path = Path.Source.of_string lock_dir_path in
  Lock_dir.Write_disk.(
    prepare ~portable_lock_dir:false ~lock_dir_path ~files:Package_name.Map.empty lock_dir
    |> commit);
  let lock_dir_round_tripped =
    try Lock_dir.read_disk_exn lock_dir_path with
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
  let lock_dir_round_tripped', lock_dir' =
    Lock_dir.remove_locs lock_dir_round_tripped, Lock_dir.remove_locs lock_dir
  in
  if Lock_dir.equal lock_dir_round_tripped' lock_dir'
  then print_endline "lockdir matches after roundtrip:"
  else (
    print_endline "lockdir doesn't match after roundtrip:";
    print_endline (Lock_dir.to_dyn lock_dir |> Dyn.to_string));
  let dyn_lock_dir = Lock_dir.to_dyn lock_dir_round_tripped in
  let dyn_lock_dir =
    match commit with
    | None -> dyn_lock_dir
    | Some commit -> Update.update_lock_dir_dyn ~commit dyn_lock_dir
  in
  print_endline (dyn_lock_dir |> Dyn.to_string)
;;

let run thunk =
  let on_event _config _event = () in
  let config : Scheduler.Config.t =
    { concurrency = 1; stats = None; print_ctrl_c_warning = false; watch_exclusions = [] }
  in
  Scheduler.Run.go config ~on_event thunk
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
         ~expanded_solver_variable_bindings:Expanded_variable_bindings.empty
         ~solved_for_platform:None)
    ();
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
    ; solved_for_platforms = ("<none>:1", [])
    }
    |}]
;;

let empty_package name ~version =
  { Lock_dir.Pkg.build_command = Lock_dir.Conditional_choice.empty
  ; install_command = Lock_dir.Conditional_choice.empty
  ; depends = Lock_dir.Conditional_choice.empty
  ; depexts = []
  ; info =
      { Lock_dir.Pkg_info.name
      ; version
      ; dev = false
      ; avoid = false
      ; source = None
      ; extra_sources = []
      }
  ; exported_env = []
  ; enabled_on_platforms = []
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
           { Expanded_variable_bindings.variable_values =
               [ Package_variable_name.os, Variable_value.string "linux" ]
           ; unset_variables = [ Package_variable_name.os_family ]
           }
         ~solved_for_platform:None
         (Package_name.Map.of_list_exn
            [ mk_pkg_basic ~name:"foo" ~version:(Package_version.of_string "0.1.0")
            ; mk_pkg_basic ~name:"bar" ~version:(Package_version.of_string "0.2.0")
            ]))
    ();
  [%expect
    {|
    lockdir matches after roundtrip:
    { version = (0, 1)
    ; dependency_hash = None
    ; packages =
        map
          { "bar" :
              map
                { "0.2.0" :
                    { build_command = []
                    ; install_command = []
                    ; depends = []
                    ; depexts = []
                    ; info =
                        { name = "bar"
                        ; version = "0.2.0"
                        ; dev = false
                        ; avoid = false
                        ; source = None
                        ; extra_sources = []
                        }
                    ; exported_env = []
                    ; enabled_on_platforms = []
                    }
                }
          ; "foo" :
              map
                { "0.1.0" :
                    { build_command = []
                    ; install_command = []
                    ; depends = []
                    ; depexts = []
                    ; info =
                        { name = "foo"
                        ; version = "0.1.0"
                        ; dev = false
                        ; avoid = false
                        ; source = None
                        ; extra_sources = []
                        }
                    ; exported_env = []
                    ; enabled_on_platforms = []
                    }
                }
          }
    ; ocaml = Some ("simple_lock_dir/lock.dune:3", "ocaml")
    ; repos = { complete = true; used = None }
    ; expanded_solver_variable_bindings =
        { variable_values = [ ("os", "linux") ]
        ; unset_variables = [ "os-family" ]
        }
    ; solved_for_platforms = ("<none>:1", [])
    }
    |}]
;;

let%expect_test "encode/decode round trip test for lockdir with complex deps" =
  let module Action = Dune_lang.Action in
  let module String_with_vars = Dune_lang.String_with_vars in
  let make_conditional value =
    Lock_dir.Conditional_choice.singleton Dune_pkg.Solver_env.empty value
  in
  let lock_dir =
    let pkg_a =
      let name = Package_name.of_string "a" in
      let extra_source : Source.t =
        Source.external_copy (Loc.none, Path.External.of_string "/tmp/a")
      in
      ( name
      , let pkg = empty_package name ~version:(Package_version.of_string "0.1.0") in
        { pkg with
          build_command =
            make_conditional
              (Lock_dir.Build_command.Action
                 Action.(Progn [ Echo [ String_with_vars.make_text Loc.none "hello" ] ]))
        ; install_command =
            make_conditional
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
                  , { url = Loc.none, OpamUrl.of_string "file://randomurl"
                    ; checksum = None
                    } )
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
          install_command = Lock_dir.Conditional_choice.empty
        ; depends = make_conditional [ { Dependency.loc = Loc.none; name = fst pkg_a } ]
        ; info =
            { pkg.info with
              dev = true
            ; source =
                Some
                  { url = Loc.none, OpamUrl.of_string "https://github.com/foo/b"
                  ; checksum =
                      Some
                        ( Loc.none
                        , Checksum.of_string
                            "sha256=adfc38f14c0188a2ad80d61451d011d27ab8839b717492d7ad42f7cb911c54c3"
                        )
                  }
            }
        } )
    in
    let pkg_c =
      let name = Package_name.of_string "c" in
      ( name
      , let pkg = empty_package name ~version:(Package_version.of_string "0.2") in
        { pkg with
          depends =
            make_conditional
              [ { Dependency.loc = Loc.none; name = fst pkg_a }
              ; { Dependency.loc = Loc.none; name = fst pkg_b }
              ]
        ; info =
            { pkg.info with
              dev = false
            ; source =
                Some
                  { url = Loc.none, OpamUrl.of_string "https://github.com/foo/c"
                  ; checksum = None
                  }
            }
        } )
    in
    let opam_repo =
      let source = Some "https://github.com/ocaml/dune" in
      Opam_repo.Private.create ~source
    in
    Lock_dir.create_latest_version
      ~local_packages:[]
      ~ocaml:(Some (Loc.none, Package_name.of_string "ocaml"))
      ~repos:(Some [ opam_repo ])
      ~expanded_solver_variable_bindings:Expanded_variable_bindings.empty
      ~solved_for_platform:None
      (Package_name.Map.of_list_exn [ pkg_a; pkg_b; pkg_c ])
  in
  lock_dir_encode_decode_round_trip_test ~lock_dir_path:"complex_lock_dir" ~lock_dir ();
  [%expect
    {|
    lockdir matches after roundtrip:
    { version = (0, 1)
    ; dependency_hash = None
    ; packages =
        map
          { "a" :
              map
                { "0.1.0" :
                    { build_command =
                        [ { condition = [ map {} ]
                          ; value = Action [ "progn"; [ "echo"; "hello" ] ]
                          }
                        ]
                    ; install_command =
                        [ { condition = [ map {} ]
                          ; value = [ "system"; "echo 'world'" ]
                          }
                        ]
                    ; depends = []
                    ; depexts = []
                    ; info =
                        { name = "a"
                        ; version = "0.1.0"
                        ; dev = false
                        ; avoid = false
                        ; source =
                            Some { url = "file:///tmp/a"; checksum = None }
                        ; extra_sources =
                            [ ("one", { url = "file:///tmp/a"; checksum = None })
                            ; ("two",
                               { url = "file://randomurl"; checksum = None })
                            ]
                        }
                    ; exported_env = [ { op = "="; var = "foo"; value = "bar" } ]
                    ; enabled_on_platforms = []
                    }
                }
          ; "b" :
              map
                { "dev" :
                    { build_command = []
                    ; install_command = []
                    ; depends =
                        [ { condition = [ map {} ]
                          ; value =
                              [ { loc = "complex_lock_dir/b.pkg:3"; name = "a" }
                              ]
                          }
                        ]
                    ; depexts = []
                    ; info =
                        { name = "b"
                        ; version = "dev"
                        ; dev = true
                        ; avoid = false
                        ; source =
                            Some
                              { url = "https://github.com/foo/b"
                              ; checksum =
                                  Some
                                    "sha256=adfc38f14c0188a2ad80d61451d011d27ab8839b717492d7ad42f7cb911c54c3"
                              }
                        ; extra_sources = []
                        }
                    ; exported_env = []
                    ; enabled_on_platforms = []
                    }
                }
          ; "c" :
              map
                { "0.2" :
                    { build_command = []
                    ; install_command = []
                    ; depends =
                        [ { condition = [ map {} ]
                          ; value =
                              [ { loc = "complex_lock_dir/c.pkg:3"; name = "a" }
                              ; { loc = "complex_lock_dir/c.pkg:3"; name = "b" }
                              ]
                          }
                        ]
                    ; depexts = []
                    ; info =
                        { name = "c"
                        ; version = "0.2"
                        ; dev = false
                        ; avoid = false
                        ; source =
                            Some
                              { url = "https://github.com/foo/c"
                              ; checksum = None
                              }
                        ; extra_sources = []
                        }
                    ; exported_env = []
                    ; enabled_on_platforms = []
                    }
                }
          }
    ; ocaml = Some ("complex_lock_dir/lock.dune:3", "ocaml")
    ; repos =
        { complete = true
        ; used = Some [ opam_repo_serializable "https://github.com/ocaml/dune" ]
        }
    ; expanded_solver_variable_bindings =
        { variable_values = []; unset_variables = [] }
    ; solved_for_platforms = ("<none>:1", [])
    }
    |}]
;;

let%expect_test "encode/decode round trip test with locked repo revision" =
  let open Fiber.O in
  let module Action = Dune_lang.Action in
  let module String_with_vars = Dune_lang.String_with_vars in
  run (fun () ->
    let cwd = Path.External.cwd () |> Path.external_ in
    let other_dir = Path.relative cwd "random-git-repo" in
    let+ git_hash = Rev_store_tests.create_repo_at other_dir in
    let lock_dir =
      let pkg_a =
        let name = Package_name.of_string "a" in
        name, empty_package name ~version:(Package_version.of_string "0.1.0")
      in
      let pkg_b =
        let name = Package_name.of_string "b" in
        name, empty_package name ~version:(Package_version.of_string "dev")
      in
      let pkg_c =
        let name = Package_name.of_string "c" in
        name, empty_package name ~version:(Package_version.of_string "0.2")
      in
      let opam_repo =
        let source = Some (sprintf "https://github.com/ocaml/dune#%s" git_hash) in
        Opam_repo.Private.create ~source
      in
      Lock_dir.create_latest_version
        ~local_packages:[]
        ~ocaml:(Some (Loc.none, Package_name.of_string "ocaml"))
        ~repos:(Some [ opam_repo ])
        ~expanded_solver_variable_bindings:Expanded_variable_bindings.empty
        ~solved_for_platform:None
        (Package_name.Map.of_list_exn [ pkg_a; pkg_b; pkg_c ])
    in
    lock_dir_encode_decode_round_trip_test
      ~commit:git_hash
      ~lock_dir_path:"complex_lock_dir"
      ~lock_dir
      ());
  [%expect
    {|
    lockdir matches after roundtrip:
    { version = (0, 1)
    ; dependency_hash = None
    ; packages =
        map
          { "a" :
              map
                { "0.1.0" :
                    { build_command = []
                    ; install_command = []
                    ; depends = []
                    ; depexts = []
                    ; info =
                        { name = "a"
                        ; version = "0.1.0"
                        ; dev = false
                        ; avoid = false
                        ; source = None
                        ; extra_sources = []
                        }
                    ; exported_env = []
                    ; enabled_on_platforms = []
                    }
                }
          ; "b" :
              map
                { "dev" :
                    { build_command = []
                    ; install_command = []
                    ; depends = []
                    ; depexts = []
                    ; info =
                        { name = "b"
                        ; version = "dev"
                        ; dev = false
                        ; avoid = false
                        ; source = None
                        ; extra_sources = []
                        }
                    ; exported_env = []
                    ; enabled_on_platforms = []
                    }
                }
          ; "c" :
              map
                { "0.2" :
                    { build_command = []
                    ; install_command = []
                    ; depends = []
                    ; depexts = []
                    ; info =
                        { name = "c"
                        ; version = "0.2"
                        ; dev = false
                        ; avoid = false
                        ; source = None
                        ; extra_sources = []
                        }
                    ; exported_env = []
                    ; enabled_on_platforms = []
                    }
                }
          }
    ; ocaml = Some ("complex_lock_dir/lock.dune:3", "ocaml")
    ; repos =
        { complete = true
        ; used =
            Some
              [ opam_repo_serializable
                  "https://github.com/ocaml/dune#MATCHES_EXPECTED"
              ]
        }
    ; expanded_solver_variable_bindings =
        { variable_values = []; unset_variables = [] }
    ; solved_for_platforms = ("<none>:1", [])
    }
    |}]
;;
