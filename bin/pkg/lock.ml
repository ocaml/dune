open Import

let run_lock_command ~(common : Common.t) ~config =
  let open Fiber.O in
  let once () =
    let request (setup : Import.Main.build_system) =
      let dir = Path.(relative root) (Common.prefix_target common ".") in
      Alias.in_dir
        ~name:Dune_rules.Alias.pkg_lock
        ~recursive:true
        ~contexts:setup.contexts
        dir
      |> Alias.request
    in
    Build.run_build_system ~common ~request
    >>| function
    | Ok () -> ()
    | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
  in
  Scheduler.go_with_rpc_server ~common ~config once
;;

let doc = "Create a lock directory."

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune pkg lock) runs the solver and creates a lock directory.|}
  ; `Blocks Common.help_secs
  ]
;;

let command =
  let term =
    let+ builder = Common.Builder.term in
    let common, config = Common.init builder in
    run_lock_command ~common ~config
  in
  Cmd.v (Cmd.info "lock" ~doc ~man ~envs:Common.envs) term
;;
