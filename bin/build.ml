open Import
module Run_id = Dune_scheduler.Run_id

let run_build_system ~request =
  Dune_engine.Build_system.run_action_builder
    (let open Action_builder.O in
     Action_builder.of_memo (Util.setup ()) >>= request)
;;

let poll_handling_rpc_build_requests ~(common : Common.t) =
  let open Fiber.O in
  let rpc =
    match Common.rpc common with
    | `Allow server -> server
    | `Forbid_builds -> Code_error.raise "rpc server must be allowed in passive mode" []
  in
  Scheduler.Run.poll_passive
    ~get_build_request:
      (let+ { kind; outcome } = Dune_rpc_impl.Server.pending_action rpc in
       let request setup =
         let root = Common.root common in
         match kind with
         | Build targets -> Target.interpret_targets (Common.root common) setup targets
         | Runtest test_paths ->
           Runtest_common.make_request
             ~scontexts:setup.scontexts
             ~to_cwd:root.to_cwd
             ~test_paths
       in
       run_build_system ~request, outcome)
;;

let run_build_command_poll_eager ~(common : Common.t) ~config ~request : unit =
  Scheduler_setup.go_with_rpc_server_and_console_status_reporting
    ~common
    ~config
    (fun () ->
       let open Fiber.O in
       (* Run two fibers concurrently. One is responible for rebuilding targets
       named on the command line in reaction to file system changes. The other
       is responsible for building targets named in RPC build requests. *)
       let+ () = Scheduler.Run.poll (run_build_system ~request)
       and+ () = poll_handling_rpc_build_requests ~common in
       ())
;;

let run_build_command_poll_passive ~common ~config ~request:_ : unit =
  (* CR-someday aalekseyev: It would've been better to complain if [request] is
     non-empty, but we can't check that here because [request] is a function.*)
  Scheduler_setup.go_with_rpc_server_and_console_status_reporting
    ~common
    ~config
    (fun () -> poll_handling_rpc_build_requests ~common)
;;

let run_build_command_once ~(common : Common.t) ~config ~request =
  let open Fiber.O in
  let once () =
    let run_id = Run_id.Batch in
    let start = Time.now () in
    Dune_trace.emit Build (fun () ->
      Dune_trace.Event.watch_build_start
        ~run_id:(Run_id.to_int run_id)
        ~restart:false
        ~files:None
        ~start);
    let+ res = run_build_system ~request in
    let stop = Time.now () in
    let outcome =
      match res with
      | Ok () -> `Success
      | Error `Already_reported -> `Failure
    in
    Dune_trace.emit Build (fun () ->
      Dune_trace.Event.watch_build_finish
        ~run_id:(Run_id.to_int run_id)
        ~outcome
        ~start
        ~stop
        ~restart_duration:None);
    match res with
    | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
    | Ok () -> ()
  in
  Scheduler_setup.go_with_rpc_server ~common ~config once
;;

let run_build_command ~(common : Common.t) ~config ~request =
  (match Common.watch common with
   | Yes Eager -> run_build_command_poll_eager
   | Yes Passive -> run_build_command_poll_passive
   | No -> run_build_command_once)
    ~common
    ~config
    ~request
;;

let build =
  let doc = "Build the given targets, or the default ones if none are given." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Targets starting with a $(b,@) are interpreted as aliases.|}
    ; `Blocks Common.help_secs
    ; Common.examples
        [ "Build all targets in the current source tree", "dune build"
        ; "Build targets in the `./foo/bar' directory", "dune build ./foo/bar"
        ; ( "Build the minimal set of targets required for tooling such as Merlin \
             (useful for quickly detecting errors)"
          , "dune build @check" )
        ; "Run all code formatting tools in-place", "dune build --auto-promote @fmt"
        ]
    ]
  in
  (* CR-someday Alizter: document this option *)
  let name_ = Arg.info [] ~docv:"TARGET" ~doc:None in
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
                  all its subdirectories. Repeatable."))
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
                  $(b,foo) alias in $(b,dir/) only. Repeatable."))
    in
    let targets = List.concat [ targets; aliases; aliases_rec ] in
    let targets =
      match targets with
      | [] -> [ Common.Builder.default_target builder ]
      | _ :: _ -> targets
    in
    let common, config = Common.init builder in
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
    match Global_lock.lock ~timeout:None with
    | Error lock_held_by ->
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
