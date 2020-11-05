open Stdune
open Import

let make_setup common =
  let rpc = Common.rpc common in
  let build_mutex = Option.map rpc ~f:Dune_rpc_impl.Server.build_mutex in
  Import.Main.setup ?build_mutex common

let run_build_command_poll ~(common : Common.t) ~targets ~setup =
  let open Fiber.O in
  let once () =
    Cached_digest.invalidate_cached_timestamps ();
    let* setup = Memo.Build.run (setup ()) in
    match
      match
        let open Option.O in
        let* rpc = Common.rpc common in
        Dune_rpc_impl.Server.pending_build_action rpc
      with
      | None -> `Build (targets setup, None)
      | Some Shutdown -> `Shutdown
      | Some (Build (targets, ivar)) ->
        `Build (Target.resolve_targets_exn common setup targets, Some ivar)
    with
    | `Shutdown -> Fiber.return `Stop
    | `Build (targets, ivar) ->
      let* () =
        match ivar with
        | None -> Fiber.return ()
        | Some ivar -> Fiber.Ivar.fill ivar Accepted
      in
      let+ () = Memo.Build.run (do_build targets) in
      `Continue
  in
  Scheduler.poll ~common ~once ~finally:Hooks.End_of_build.run

let run_build_command_once ~(common : Common.t) ~targets ~setup =
  let once () =
    let open Fiber.O in
    let* setup = Memo.Build.run (setup ()) in
    let targets = targets setup in
    Memo.Build.run (do_build targets)
  in
  Scheduler.go ~common once

let run_build_command ~(common : Common.t) ~targets =
  let setup () = make_setup common in
  ( if Common.watch common then
    run_build_command_poll
  else
    run_build_command_once )
    ~setup ~common ~targets;
  Build_system.cache_teardown ()

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
