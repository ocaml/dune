open Stdune
module Checksum = Dune_pkg.Checksum
module Lock_dir = Dune_pkg.Lock_dir
module Opam_repo = Dune_pkg.Opam_repo
module Expanded_variable_bindings = Dune_pkg.Solver_stats.Expanded_variable_bindings
module Variable_name = Dune_pkg.Variable_name
module Variable_value = Dune_pkg.Variable_value
module Rev_store = Dune_pkg.Rev_store
module Package_version = Dune_pkg.Package_version
module Package_name = Dune_lang.Package_name
module Scheduler = Dune_engine.Scheduler

let () = Dune_tests_common.init ()

module Update = struct
  open Dyn

  let update_commit ~commit = function
    | Option (Some (Variant ("Commit", [ String s ]))) as d ->
      (match String.equal commit s with
       | true -> Option (Some (Variant ("Commit", [ string "MATCHES EXPECTED" ])))
       | false -> d)
    | otherwise -> otherwise
  ;;

  let update_source ~commit = function
    | Record xs ->
      let xs =
        List.map xs ~f:(function
          | ("commit" as u), dyn -> u, update_commit ~commit dyn
          | otherwise -> otherwise)
      in
      Record xs
    | otherwise -> otherwise
  ;;

  let update_repo_id ~commit = function
    | Option (Some (Variant ("Git_hash", [ String s ]))) as d ->
      (match String.equal commit s with
       | true -> Option (Some (Variant ("Git_hash", [ string "MATCHES EXPECTED" ])))
       | false -> d)
    | otherwise -> otherwise
  ;;

  let update_used ~commit = function
    | Option (Some (List xs)) ->
      let xs =
        List.map xs ~f:(function
          | Variant (("opam_repo_serializable" as u), [ repo_id; source ]) ->
            let repo_id = update_repo_id ~commit repo_id in
            let source = update_source ~commit source in
            Variant (u, [ repo_id; source ])
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
  let lock_dir_round_tripped', lock_dir' =
    match commit with
    | None -> Lock_dir.remove_locs lock_dir_round_tripped, Lock_dir.remove_locs lock_dir
    | Some commit ->
      let lock_dir_round_tripped = Lock_dir.remove_locs lock_dir_round_tripped in
      let lock_dir = Lock_dir.remove_locs lock_dir in
      ( Lock_dir.Private.with_commit ~commit lock_dir_round_tripped
      , Lock_dir.Private.with_commit ~commit lock_dir )
  in
  if Lock_dir.equal lock_dir_round_tripped' lock_dir'
  then print_endline "lockdir matches after roundtrip:"
  else print_endline "lockdir doesn't match after roundtrip:";
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
    { concurrency = 1
    ; stats = None
    ; insignificant_changes = `Ignore
    ; signal_watcher = `No
    ; watch_exclusions = []
    }
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
         ~expanded_solver_variable_bindings:Expanded_variable_bindings.empty)
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
           { Expanded_variable_bindings.variable_values =
               [ Variable_name.os, Variable_value.string "linux" ]
           ; unset_variables = [ Variable_name.os_family ]
           }
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
        { variable_values = [ ("os", "linux") ]
        ; unset_variables = [ "os-family" ]
        }
    } |}]
;;

let%expect_test "encode/decode round trip test for lockdir with complex deps" =
  let open Fiber.O in
  let module Action = Dune_lang.Action in
  let module String_with_vars = Dune_lang.String_with_vars in
  run (fun () ->
    let+ lock_dir =
      let pkg_a =
        let name = Package_name.of_string "a" in
        let extra_source : Lock_dir.Source.t =
          External_copy (Loc.none, Path.External.of_string "/tmp/a")
        in
        ( name
        , let pkg = empty_package name ~version:(Package_version.of_string "0.1.0") in
          { pkg with
            build_command =
              Some Action.(Progn [ Echo [ String_with_vars.make_text Loc.none "hello" ] ])
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
                    (Fetch { url = Loc.none, "https://github.com/foo/c"; checksum = None })
              }
          } )
      in
      let+ opam_repo =
        let repo_id = Some (Dune_pkg.Repository_id.of_git_hash "95cf548dc") in
        let+ source =
          OpamUrl.parse "https://github.com/ocaml/dune"
          |> Opam_repo.Source.of_opam_url
          >>| (fun (src : Opam_repo.Source.t) -> src.url)
          >>| Option.some
        in
        Opam_repo.Private.create ~source ~repo_id
      in
      Lock_dir.create_latest_version
        ~local_packages:[]
        ~ocaml:(Some (Loc.none, Package_name.of_string "ocaml"))
        ~repos:(Some [ opam_repo ])
        ~expanded_solver_variable_bindings:Expanded_variable_bindings.empty
        (Package_name.Map.of_list_exn [ pkg_a; pkg_b; pkg_c ])
    in
    lock_dir_encode_decode_round_trip_test ~lock_dir_path:"complex_lock_dir" ~lock_dir ());
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
                  "https://github.com/ocaml/dune"
              ]
        }
    ; expanded_solver_variable_bindings =
        { variable_values = []; unset_variables = [] }
    } |}]
;;

let%expect_test "encode/decode round trip test with locked repo revision" =
  let open Fiber.O in
  let module Action = Dune_lang.Action in
  let module String_with_vars = Dune_lang.String_with_vars in
  run (fun () ->
    let cwd = Path.External.cwd () |> Path.external_ in
    let dir = Path.relative cwd "git-repo" in
    let* git_hash = Rev_store_tests.create_repo_at dir in
    let* rev_store = Rev_store.load_or_create ~dir in
    let+ lock_dir =
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
      let+ opam_repo =
        let repo_id = Some (Dune_pkg.Repository_id.of_git_hash git_hash) in
        let+ source =
          sprintf "https://github.com/ocaml/dune#%s" git_hash
          |> OpamUrl.parse
          |> Opam_repo.Source.Private.of_opam_url rev_store
          >>| (fun (src : Opam_repo.Source.t) -> src.url)
          >>| Option.some
        in
        Opam_repo.Private.create ~source ~repo_id
      in
      Lock_dir.create_latest_version
        ~local_packages:[]
        ~ocaml:(Some (Loc.none, Package_name.of_string "ocaml"))
        ~repos:(Some [ opam_repo ])
        ~expanded_solver_variable_bindings:Expanded_variable_bindings.empty
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
              { build_command = None
              ; install_command = None
              ; deps = []
              ; info =
                  { name = "a"
                  ; version = "0.1.0"
                  ; dev = false
                  ; source = None
                  ; extra_sources = []
                  }
              ; exported_env = []
              }
          ; "b" :
              { build_command = None
              ; install_command = None
              ; deps = []
              ; info =
                  { name = "b"
                  ; version = "dev"
                  ; dev = false
                  ; source = None
                  ; extra_sources = []
                  }
              ; exported_env = []
              }
          ; "c" :
              { build_command = None
              ; install_command = None
              ; deps = []
              ; info =
                  { name = "c"
                  ; version = "0.2"
                  ; dev = false
                  ; source = None
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
                  Some Git_hash "MATCHES EXPECTED",
                  "https://github.com/ocaml/dune"
              ]
        }
    ; expanded_solver_variable_bindings =
        { variable_values = []; unset_variables = [] }
    } |}]
;;
