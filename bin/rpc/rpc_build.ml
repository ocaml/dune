open Import

(* Sends a request to build [targets] to the RPC server at [where]. The targets
   are specified as strings containing sexp-encoded targets that are passed to
   this command as arguments on the command line. *)
let build_sexp_string_targets ~wait ~targets =
  let open Fiber.O in
  let* connection = Rpc_common.establish_client_session ~wait in
  Dune_rpc_impl.Client.client
    connection
    (Dune_rpc.Initialize.Request.create ~id:(Dune_rpc.Id.make (Sexp.Atom "build")))
    ~f:(fun session ->
      Rpc_common.request_exn
        session
        (Dune_rpc_private.Decl.Request.witness Dune_rpc_impl.Decl.build)
        targets)
;;

let build ~wait targets =
  let targets =
    List.map targets ~f:(fun target ->
      let sexp = Dune_lang.Dep_conf.encode target in
      Dune_lang.to_string sexp)
  in
  build_sexp_string_targets ~wait ~targets
;;

let term =
  let name_ = Arg.info [] ~docv:"TARGET" in
  let+ (builder : Common.Builder.t) = Common.Builder.term
  and+ wait = Rpc_common.wait_term
  and+ targets = Arg.(value & pos_all string [] name_) in
  Rpc_common.client_term builder
  @@ fun _common ->
  let open Fiber.O in
  let+ response = build_sexp_string_targets ~wait ~targets in
  match response with
  | Error (error : Dune_rpc_private.Response.Error.t) ->
    Printf.eprintf
      "Error: %s\n%!"
      (Dyn.to_string (Dune_rpc_private.Response.Error.to_dyn error))
  | Ok Success -> print_endline "Success"
  | Ok (Failure _) -> print_endline "Failure"
;;

let info =
  let doc =
    "build a given target (requires dune to be running in passive watching mode)"
  in
  Cmd.info "build" ~doc
;;

let cmd = Cmd.v info term
