open Import

let exec () =
  let open Fiber.O in
  let where = Rpc_common.active_server () in
  let module Client = Dune_rpc_client.Client in
  let+ errors =
    let* connect = Client.Connection.connect_exn where in
    Dune_rpc_impl.Client.client
      connect
      (Dune_rpc_private.Initialize.Request.create
         ~id:(Dune_rpc_private.Id.make (Sexp.Atom "diagnostics_cmd")))
      ~f:(fun cli ->
        let* decl =
          Client.Versioned.prepare_request cli Dune_rpc_private.Public.Request.diagnostics
        in
        match decl with
        | Error e -> raise (Dune_rpc_private.Version_error.E e)
        | Ok decl -> Client.request cli decl ())
  in
  match errors with
  | Ok errors ->
    List.iter errors ~f:(fun err ->
      Console.print_user_message (Dune_rpc.Diagnostic.to_user_message err))
  | Error e -> Rpc_common.raise_rpc_error e
;;

let info =
  let doc = "Fetch and return errors from the current build." in
  Cmd.info "diagnostics" ~doc
;;

let term =
  let+ (builder : Common.Builder.t) = Common.Builder.term in
  Rpc_common.client_term builder exec
;;

let command = Cmd.v info term
