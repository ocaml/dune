module Rpc = Dune_rpc.Private
module Protocol = Rpc.Rule_plugin
module Sexp = Protocol.Sexp

module Make
    (Fiber : Rpc.Fiber)
    (Chan : sig
       type t
     end)
    (Client : Rpc.Client.S with type 'a fiber := 'a Fiber.t and type chan := Chan.t) =
struct
  let run chan ~f =
    let id =
      match Sys.getenv_opt Protocol.instance_id_env_var with
      | Some encoded ->
        (match Csexp.parse_string encoded with
         | Ok id -> Rpc.Id.make id
         | Error (_, message) -> invalid_arg message)
      | None ->
        invalid_arg
          (Printf.sprintf
             "%s is not set; this program must be started by Dune"
             Protocol.instance_id_env_var)
    in
    let initialize = Rpc.Initialize.Request.create ~id in
    let private_menu = [ Client.Handle_request (Protocol.load_rules, f) ] in
    Client.connect_with_menu ~private_menu chan initialize ~f:Client.disconnected
  ;;
end
