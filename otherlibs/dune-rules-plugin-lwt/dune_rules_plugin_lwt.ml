module Sexp = Dune_rules_plugin.Sexp

module Chan = struct
  type t = Lwt_io.input_channel * Lwt_io.output_channel
end

module Plugin =
  Dune_rules_plugin.Make (Dune_rpc_lwt.V1.Private.Fiber) (Chan)
    (Dune_rpc_lwt.V1.Private.Client)

let run load_rules =
  let open Lwt.Infix in
  let open Lwt.Syntax in
  let* where =
    Dune_rpc_lwt.V1.Where.get ~env:Sys.getenv_opt ~build_dir:"_build"
    >|= function
    | Ok (Some where) -> where
    | Ok None -> invalid_arg "this program must be started by Dune"
    | Error exn -> raise exn
  in
  Dune_rpc_lwt.V1.connect_chan where >>= Plugin.run ~f:load_rules
;;
