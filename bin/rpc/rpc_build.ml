open Import

let term =
  (* CR-someday Alizter: document this option *)
  let name_ = Arg.info [] ~docv:"TARGET" ~doc:None in
  let+ (builder : Common.Builder.t) = Common.Builder.term
  and+ wait = Rpc_common.wait_term
  and+ targets = Arg.(value & pos_all string [] name_) in
  Rpc_common.client_term builder
  @@ fun () ->
  let open Fiber.O in
  let+ response =
    Rpc_common.fire_request ~name:"build" ~wait builder Dune_rpc_impl.Decl.build targets
  in
  match response with
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
