open Import

let prepare_targets targets =
  List.map targets ~f:(fun target ->
    let sexp = Dune_lang.Dep_conf.encode target in
    Dune_lang.to_string sexp)
;;

let term =
  let name_ = Arg.info [] ~docv:"TARGET" in
  let+ (builder : Common.Builder.t) = Common.Builder.term
  and+ wait = Rpc_common.wait_term
  and+ targets = Arg.(value & pos_all string [] name_) in
  Rpc_common.client_term builder
  @@ fun () ->
  let open Fiber.O in
  let+ build_outcome =
    Rpc_common.fire_message
      ~name:"build"
      ~wait
      builder
      (Rpc_common.Request (Dune_rpc.Decl.Request.witness Dune_rpc_impl.Decl.build))
      targets
  in
  match build_outcome with
  (* This is the only output that the client will see, details are in the server. *)
  | Success -> print_endline "Success"
  | Failure _ -> print_endline "Failure"
;;

let info =
  let doc =
    "build a given target (requires dune to be running in passive watching mode)"
  in
  Cmd.info "build" ~doc
;;

let cmd = Cmd.v info term
