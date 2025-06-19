(** dune rpc command group *)
val group : unit Cmdliner.Cmd.t

module Build = Build
module Runtest = Rpc_runtest
