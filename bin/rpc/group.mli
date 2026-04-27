(** dune rpc command group *)
val group : unit Cmdliner.Cmd.t

module Build = Rpc_build
module Flush_file_watcher = Rpc_flush_file_watcher
