(** [dune rpc flush-file-watcher] flushes pending file watcher notifications
    in a running watch server. *)
val cmd : unit Cmdliner.Cmd.t
