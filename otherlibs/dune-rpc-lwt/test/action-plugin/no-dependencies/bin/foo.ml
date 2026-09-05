open Dune_rpc_lwt.V1.Action_plugin

let action _ = Lwt_io.printl "Hello from foo!"
let () = Lwt_main.run (run action)
