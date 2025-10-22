open Import

let build ~wait builder lock_held_by targets =
  let targets =
    List.map targets ~f:(fun target ->
      let sexp = Dune_lang.Dep_conf.encode target in
      Dune_lang.to_string sexp)
  in
  Rpc_common.fire_request
    ~name:"build"
    ~wait
    ~lock_held_by
    builder
    Dune_rpc_impl.Decl.build
    targets
;;

let term =
  let name_ = Arg.info [] ~docv:"TARGET" in
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
  | Error (error : Dune_rpc.Response.Error.t) ->
    Printf.eprintf "Error: %s\n%!" (Dyn.to_string (Dune_rpc.Response.Error.to_dyn error))
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
