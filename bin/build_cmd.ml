open Stdune
open Import

let run_build_command ~common ~targets =
  let once () =
    let open Fiber.O in
    let* setup = Import.Main.setup common in
    do_build (targets setup)
  in
  if Common.watch common then
    let once () =
      Cached_digest.invalidate_cached_timestamps ();
      once ()
    in
    Scheduler.poll ~common ~once ~finally:Hooks.End_of_build.run ()
  else
    Scheduler.go ~common once;
  match Build_system.get_cache () with
  | Some { cache = (module Caching : Cache.Caching); _ } ->
    (* Synchronously wait for the end of the connection with the cache daemon,
       ensuring all dedup messages have been queued. *)
    Caching.Cache.teardown Caching.cache;
    (* Hande all remaining dedup messages. *)
    Scheduler.wait_for_dune_cache ()
  | None -> ()

let runtest =
  let doc = "Run tests." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This is a short-hand for calling:|}
    ; `Pre {|  dune build @runtest|}
    ; `Blocks Common.help_secs
    ; Common.examples
        [ ( "Run all tests in the current source tree (including those that \
             passed on the last run)"
          , "dune runtest --force" )
        ; ( "Run tests sequentially without output buffering"
          , "dune runtest --no-buffer -j 1" )
        ]
    ]
  in
  let name_ = Arg.info [] ~docv:"DIR" in
  let term =
    let+ common = Common.term
    and+ dirs = Arg.(value & pos_all string [ "." ] name_) in
    Common.set_common common
      ~targets:
        (List.map dirs ~f:(fun s ->
             let dir = Path.Local.of_string s in
             Arg.Dep.alias_rec ~dir Dune_engine.Alias.Name.runtest));
    let targets (setup : Import.Main.build_system) =
      List.map dirs ~f:(fun dir ->
          let dir = Path.(relative root) (Common.prefix_target common dir) in
          Target.Alias
            (Alias.in_dir ~name:Dune_engine.Alias.Name.runtest ~recursive:true
               ~contexts:setup.workspace.contexts dir))
    in
    run_build_command ~common ~targets
  in
  (term, Term.info "runtest" ~doc ~man)

let build =
  let doc =
    "Build the given targets, or all installable targets if none are given."
  in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Targets starting with a $(b,@) are interpreted as aliases.|}
    ; `Blocks Common.help_secs
    ; Common.examples
        [ ("Build all targets in the current source tree", "dune build")
        ; ("Build targets in the `./foo/bar' directory", "dune build ./foo/bar")
        ; ( "Build the minimal set of targets required for tooling such as \
             Merlin (useful for quickly detecting errors)"
          , "dune build @check" )
        ; ( "Run all code formatting tools in-place"
          , "dune build --auto-promote @fmt" )
        ]
    ]
  in
  let name_ = Arg.info [] ~docv:"TARGET" in
  let term =
    let+ common = Common.term
    and+ targets = Arg.(value & pos_all dep [] name_) in
    let targets =
      match targets with
      | [] -> [ Common.default_target common ]
      | _ :: _ -> targets
    in
    Common.set_common common ~targets;
    let targets setup = Target.resolve_targets_exn common setup targets in
    run_build_command ~common ~targets
  in
  (term, Term.info "build" ~doc ~man)
