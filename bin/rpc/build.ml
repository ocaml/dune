open Import
module Client = Dune_rpc_client.Client

let retry_loop once =
  let open Fiber.O in
  let rec loop () =
    let* res = once () in
    match res with
    | Some result -> Fiber.return result
    | None ->
      let* () = Scheduler.sleep 0.2 in
      loop ()
  in
  loop ()
;;

let establish_connection_or_raise ~wait once =
  let open Fiber.O in
  if wait
  then retry_loop once
  else
    let+ res = once () in
    match res with
    | Some conn -> conn
    | None ->
      let (_ : Dune_rpc_private.Where.t) = Rpc_common.active_server () in
      User_error.raise
        [ Pp.text "failed to establish connection even though server seems to be running"
        ]
;;

let establish_client_session ~wait =
  let open Fiber.O in
  let once () =
    let where = Dune_rpc_impl.Where.get () in
    match where with
    | None -> Fiber.return None
    | Some where ->
      let+ connection = Client.Connection.connect where in
      (match connection with
       | Ok conn -> Some conn
       | Error message ->
         if not wait then Console.print_user_message message;
         None)
  in
  establish_connection_or_raise ~wait once
;;

let term =
  let name_ = Arg.info [] ~docv:"TARGET" in
  let+ (builder : Common.Builder.t) = Common.Builder.term
  and+ wait = Rpc_common.wait_term
  and+ targets = Arg.(value & pos_all string [] name_) in
  Rpc_common.client_term builder
  @@ fun _common ->
  let open Fiber.O in
  let* conn = establish_client_session ~wait in
  Dune_rpc_impl.Client.client
    conn
    (Dune_rpc.Initialize.Request.create ~id:(Dune_rpc.Id.make (Sexp.Atom "build")))
    ~f:(fun session ->
      let open Fiber.O in
      let+ response =
        Rpc_common.request_exn
          session
          (Dune_rpc_private.Decl.Request.witness Dune_rpc_impl.Decl.build)
          targets
      in
      match response with
      | Error (error : Dune_rpc_private.Response.Error.t) ->
        Printf.printf
          "Error: %s\n%!"
          (Dyn.to_string (Dune_rpc_private.Response.Error.to_dyn error))
      | Ok Success -> print_endline "Success"
      | Ok Failure -> print_endline "Failure")
;;

let info =
  let doc =
    "build a given target (requires dune to be running in passive watching mode)"
  in
  Cmd.info "build" ~doc
;;

let cmd = Cmd.v info term
