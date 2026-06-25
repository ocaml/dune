open Import

let action_builder_of_request request =
  let open Action_builder.O in
  Action_builder.of_memo (Memo.of_thunk Util.setup) >>= request
;;

let run_build_system ~action_runner ~run_id ~request =
  let build =
    Dune_engine.Process.Build.create
      ~action_runner
      ~run_id
      ~cancellation:(Fiber.Cancel.create ())
  in
  let request =
    action_builder_of_request request
    |> Dune_engine.Build_system.Request.Goal.create
    |> List.singleton
    |> Dune_engine.Build_system.Request.create
  in
  Dune_engine.Build_system.run_build_requests ~build request
;;

let run_build_command_poll ~(common : Common.t) ~config ~sticky_goal : unit =
  let build_loop = Common.build_loop common in
  Scheduler_setup.go_with_rpc_server_and_file_watcher ~common ~config (fun () ->
    Dune_engine.Build_loop.run build_loop (fun () ->
      Dune_engine.Build_loop.poll
        build_loop
        ~action_runner:(Common.action_runner common)
        ~sticky_goal))
;;

let run_build_command_once ~(common : Common.t) ~config ~request =
  let open Fiber.O in
  let once () =
    run_build_system
      ~action_runner:(Common.action_runner common)
      ~run_id:Dune_engine.Run_id.Batch
      ~request
    >>| function
    | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
    | Ok () -> ()
  in
  Scheduler_setup.go_with_rpc_server ~common ~config once
;;

let run_build_command ~(common : Common.t) ~config ~request =
  match Common.watch common with
  | No -> run_build_command_once ~common ~config ~request
  | Yes kind ->
    let sticky_goal =
      match kind with
      | Eager -> Some (action_builder_of_request request)
      | Passive ->
        (* CR-someday aalekseyev: It would've been better to complain if
           [request] is non-empty, but we can't check that here because
           [request] is a function. *)
        None
    in
    run_build_command_poll ~common ~config ~sticky_goal
;;

let build =
  let doc = "Build the given targets, or the default ones if none are given." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Each $(b,TARGET) argument is one of the following forms:|}
    ; `I
        ( {|$(i,PATH)|}
        , {|A file or directory path. If a build rule produces it (including a
          directory target), that rule is run; otherwise, if $(i,PATH) is a
          source directory, dune builds the $(b,@@default) alias in that
          directory. The path may contain any percent form accepted in dune
          stanzas, such as $(b,%{bin:foo}) or $(b,%{cmi:lib/mod}).|}
        )
    ; `I
        ( {|$(b,@)$(i,name)|}
        , {|Build the alias $(i,name) in the current directory and all
          subdirectories. The name may be prefixed with a build-context path
          such as $(b,@_build/foo/runtest) to restrict to a single context.
          Equivalent to $(b,--alias-rec) $(i,name).|}
        )
    ; `I
        ( {|$(b,@@)$(i,name)|}
        , {|Build the alias $(i,name) in the current directory only. Equivalent
          to $(b,--alias) $(i,name).|}
        )
    ; `I
        ( {|$(b,\(file PATH\))|}
        , {|Equivalent to $(i,PATH), but disambiguates paths starting with
          $(b,@).|}
        )
    ; `I
        ( {|$(b,\(alias NAME\)), $(b,\(alias_rec NAME\))|}
        , {|Equivalent to $(b,@@)$(i,NAME) and $(b,@)$(i,NAME) respectively.|} )
    ; `P
        {|When no $(b,TARGET) is given, dune builds the default target
        (configurable with $(b,--default-target)).|}
    ; `P
        {|If another instance of dune is running in watch mode in the same
        workspace (started with $(b,--watch) / $(b,-w)), $(b,dune build)
        forwards the build request to it over RPC. If another instance is
        running but not in watch mode, the build is aborted.|}
    ; `Blocks Common.help_secs
    ; Common.examples
        [ "Build all targets in the current source tree", "dune build"
        ; "Build targets in the `./foo/bar' directory", "dune build ./foo/bar"
        ; ( "Build the minimal set of targets required for tooling such as Merlin \
             (useful for quickly detecting errors)"
          , "dune build @check" )
        ; "Build the public executable `foo'", "dune build %{bin:foo}"
        ; "Run all code formatting tools in-place", "dune build --auto-promote @fmt"
        ]
    ]
  in
  let name_ =
    Arg.info
      []
      ~docv:"TARGET"
      ~doc:
        (Some
           "A path, alias (such as $(b,@runtest)), or S-expression to build. See the \
            $(b,DESCRIPTION) section for the accepted forms. Can be repeated.")
  in
  let term =
    let+ builder = Common.Builder.term
    and+ targets = Arg.(value & pos_all dep [] name_)
    and+ aliases_rec =
      Arg.(
        value
        & opt_all Dep.alias_rec_arg []
        & info
            [ "alias-rec" ]
            ~docv:"ALIAS"
            ~doc:
              (Some
                 "Build the alias $(docv) in its parent directory and all \
                  subdirectories. Equivalent to the build target $(b,@)$(docv). Example: \
                  $(b,--alias-rec dir/foo) builds the $(b,foo) alias in $(b,dir/) and \
                  all its subdirectories. Can be repeated."))
    and+ aliases =
      Arg.(
        value
        & opt_all Dep.alias_arg []
        & info
            [ "alias" ]
            ~docv:"ALIAS"
            ~doc:
              (Some
                 "Build $(docv) in its parent directory only. Equivalent to the build \
                  target $(b,@@)$(docv). Example: $(b,--alias dir/foo) builds the \
                  $(b,foo) alias in $(b,dir/) only. Can be repeated."))
    in
    let targets = List.concat [ targets; aliases; aliases_rec ] in
    let targets =
      match targets with
      | [] -> [ Common.Builder.default_target builder ]
      | _ :: _ -> targets
    in
    let common, config = Common.init_build builder in
    (* Here we need to find out whether another instance of dune already holds
       the global build lock, as this will determine whether the current
       instance of dune will perform the build itself or send a build request
       to the RPC server in an already-running dune process. The method of
       checking whether another dune instance holds the lock is to simply try
       to take the lock. If taking the lock succeeds then the current process
       will perform the build itself, and future attempts by this process to
       take the lock are guaranteed to succeed. If taking the lock fails then
       we know that another instance of dune must have it, and the current
       process will send a build RPC request to that dune instance. Checking
       the status of the lock by taking prevents a race condition where the
       state of the lock could otherwise change between checking it and taking
       it. *)
    match Global_lock.lock () with
    | Error lock_held_by ->
      if Common.action_runner_requested common
      then
        User_error.raise
          [ Pp.text
              "Action runner flags cannot be used when forwarding build requests to an \
               existing Dune process. Start the server with the action runner flags \
               instead."
          ];
      (* This case is reached if dune detects that another instance of dune
         is already running. Rather than performing the build itself, the
         current instance of dune will instruct the already-running instance to
         perform the build by sending an RPC message. As only one RPC server
         can run at a time we need to use a fiber scheduler which does not run
         an RPC server in the background to schedule the fiber which will
         perform the RPC call.
      *)
      let targets = Rpc.Rpc_common.prepare_targets targets in
      Scheduler_setup.go_without_rpc_server ~common ~config (fun () ->
        let open Fiber.O in
        Rpc.Rpc_common.fire_request
          ~name:"build"
          ~wait:false
          ~lock_held_by
          builder
          Dune_rpc_impl.Decl.build
          targets
        >>| Rpc.Rpc_common.wrap_build_outcome_exn ~print_on_success:true)
    | Ok () ->
      let request setup = Target.interpret_targets (Common.root common) setup targets in
      run_build_command ~common ~config ~request
  in
  Cmd.v (Cmd.info "build" ~doc ~man ~envs:Common.envs) term
;;

let build_memo f = Build_system.run f
let build_memo_exn f = Build_system.run_exn f
