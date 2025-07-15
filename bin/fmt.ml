open Import

let doc = "Format source code."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune fmt) runs the formatter on the source code. The formatter is
        automatically selected. ocamlformat is used to format OCaml source code
        ( *.ml and *.mli files) and refmt is used to format Reason source code
        ( *.re and *.rei files).|}
  ; `Blocks Common.help_secs
  ]
;;

let format_via_rpc_server () =
  let open Fiber.O in
  let* connection = Rpc_common.establish_client_session ~wait:true in
  Dune_rpc_impl.Client.client
    connection
    (Dune_rpc.Initialize.Request.create ~id:(Dune_rpc.Id.make (Sexp.Atom "format")))
    ~f:(fun client ->
      Rpc_common.request_exn
        client
        (Dune_rpc_private.Decl.Request.witness Dune_rpc_impl.Decl.format)
        ())
;;

let format_via_rpc_server () =
  let open Fiber.O in
  format_via_rpc_server ()
  >>| function
  | Error (error : Dune_rpc_private.Response.Error.t) ->
    Printf.eprintf
      "Error: %s\n%!"
      (Dyn.to_string (Dune_rpc_private.Response.Error.to_dyn error))
  | Ok Success ->
    Console.print_user_message
      (User_message.make [ Pp.text "Success" |> Pp.tag User_message.Style.Success ])
  | Ok (Failure errors) ->
    List.iter errors ~f:(fun { Dune_engine.Compound_user_error.main; _ } ->
      Console.print_user_message main);
    User_error.raise
      [ (match List.length errors with
         | 0 ->
           Code_error.raise
             "Format via RPC failed, but the RPC server did not send an error message."
             []
         | 1 -> Pp.textf "Format failed with 1 error."
         | n -> Pp.textf "Format failed with %d errors." n)
      ]
;;

let lock_ocamlformat () =
  if Lazy.force Lock_dev_tool.is_enabled
  then
    (* Note that generating the ocamlformat lockdir here means
       that it will be created when a user runs `dune fmt` but not
       when a user runs `dune build @fmt`. It's important that
       this logic remain outside of `dune build`, as `dune
       build` is intended to only build targets, and generating
       a lockdir is not building a target. *)
    Lock_dev_tool.lock_dev_tool Ocamlformat |> Memo.run
  else Fiber.return ()
;;

let run_fmt_command ~builder =
  let open Fiber.O in
  let common, config = Common.init builder in
  let once () =
    let* () = lock_ocamlformat () in
    let request (setup : Import.Main.build_system) =
      let dir = Path.(relative root) (Common.prefix_target common ".") in
      Alias.in_dir ~name:Dune_rules.Alias.fmt ~recursive:true ~contexts:setup.contexts dir
      |> Alias.request
    in
    Build.run_build_system ~common ~request
    >>| function
    | Ok () -> ()
    | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
  in
  match Dune_util.Global_lock.lock ~timeout:None with
  | Ok () -> Scheduler.go_with_rpc_server ~common ~config once
  | Error lock_held_by ->
    Scheduler.go_without_rpc_server ~common ~config (fun () ->
      if not (Common.Builder.equal builder Common.Builder.default)
      then
        User_warning.emit
          [ Pp.textf
              "Your request is being forwarded to a running Dune instance%s so most \
               command-line arguments will be ignored."
              (match (lock_held_by : Dune_util.Global_lock.Lock_held_by.t) with
               | Unknown -> ""
               | Pid_from_lockfile pid -> sprintf " (pid: %d)" pid)
          ];
      format_via_rpc_server ())
;;

let command =
  let term =
    let+ builder = Common.Builder.term
    and+ no_promote =
      Arg.(
        value
        & flag
        & info
            [ "preview" ]
            ~doc:
              "Just print the changes that would be made without actually applying them. \
               This takes precedence over auto-promote as that flag is assumed for this \
               command.")
    in
    let builder =
      Common.Builder.set_promote builder (if no_promote then Never else Automatically)
    in
    run_fmt_command ~builder
  in
  Cmd.v (Cmd.info "fmt" ~doc ~man ~envs:Common.envs) term
;;
