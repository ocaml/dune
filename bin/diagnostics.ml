open Import

let info =
  let doc = "Fetch and return errors from the current build." in
  Cmd.info "diagnostics" ~doc
;;

let term =
  let+ (builder : Common.Builder.t) = Common.Builder.term in
  Rpc.Rpc_common.client_term builder (fun () ->
    let open Fiber.O in
    let+ errors =
      Rpc.Rpc_common.fire_message
        ~name:"diagnostics_cmd"
        ~wait:false
        builder
        (Rpc.Rpc_common.Request Dune_rpc_private.Procedures.Public.diagnostics)
        ()
    in
    List.iter errors ~f:(fun err ->
      Console.print_user_message (Dune_rpc.Diagnostic.to_user_message err)))
;;

let command = Cmd.v info term
