module Dune_rpc = Dune_rpc_private

let progress_of_build_event :
    Dune_engine.Build_config.Handler.event -> Dune_rpc.Progress.t = function
  | Start -> In_progress { complete = 0; remaining = 0 }
  | Finish -> Success
  | Interrupt -> Interrupted
  | Fail -> Failed
