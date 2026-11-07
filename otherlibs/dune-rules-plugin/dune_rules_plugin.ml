module Rpc = Dune_rpc.Private
module Protocol = Rpc.Rule_plugin
module Client = Dune_rpc_lwt.V1.Private.Client
module Sexp = Protocol.Sexp

let endpoint () =
  let missing variable =
    invalid_arg
      (Printf.sprintf "%s is not set; this program must be started by Dune" variable)
  in
  let host =
    match Sys.getenv_opt "DUNE_RULES_PLUGIN_HOST" with
    | Some host -> host
    | None -> missing "DUNE_RULES_PLUGIN_HOST"
  in
  let port =
    match Sys.getenv_opt "DUNE_RULES_PLUGIN_PORT" with
    | Some port ->
      (match int_of_string_opt port with
       | Some port -> port
       | None -> invalid_arg "DUNE_RULES_PLUGIN_PORT is not an integer")
    | None -> missing "DUNE_RULES_PLUGIN_PORT"
  in
  `Ip (`Host host, `Port port)
;;

let run load_rules_impl =
  let open Lwt.Syntax in
  Lwt_main.run
    (let* connection = Dune_rpc_lwt.V1.connect_chan (endpoint ()) in
     let private_menu =
       [ Client.Request Protocol.ready
       ; Client.Handle_request
           (Protocol.load_rules, fun arguments -> Lwt.return (load_rules_impl arguments))
       ]
     in
     let initialize =
       Rpc.Initialize.Request.create ~id:(Rpc.Id.make (Csexp.Atom "dune-rules-plugin"))
     in
     Client.connect_with_menu ~private_menu connection initialize ~f:(fun client ->
       let* ready =
         Client.Versioned.prepare_request client (Rpc.Decl.Request.witness Protocol.ready)
       in
       let ready =
         match ready with
         | Ok ready -> ready
         | Error error -> failwith (Rpc.Version_error.message error)
       in
       let* response = Client.request client ready () in
       match response with
       | Ok () -> Client.disconnected client
       | Error error -> failwith (Rpc.Response.Error.message error)))
;;
