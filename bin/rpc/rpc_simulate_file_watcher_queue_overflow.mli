(** [dune rpc simulate-file-watcher-queue-overflow] injects a queue overflow
    into a running watch server. *)
val cmd : unit Cmdliner.Cmd.t
