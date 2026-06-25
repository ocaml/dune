open Dune_rpc_lwt.V1.Action_plugin

let action _ = Lwt_io.printl "Hello from foo!"
let () = run action
