open Import
module Client = Dune_rpc_client.Client

let info =
  let doc =
    "Ping the build server running in the current directory. Passing the --wait flag \
     allows the command to wait for a connection to the server."
  in
  Cmd.info "ping" ~doc
;;

let term =
  let+ (builder : Common.Builder.t) = Common.Builder.term
  and+ wait = Common.wait_term in
  Common.client_term builder
  @@ fun () ->
  let open Fiber.O in
  Common.fire_request
    ~name:"ping_cmd"
    ~wait
    builder
    Dune_rpc_private.Procedures.Public.ping
    ()
  >>| function
  | Ok () -> Console.print [ Pp.text "Server appears to be responding normally" ]
  | Error e -> Common.raise_rpc_error e
;;

let cmd = Cmd.v info term
