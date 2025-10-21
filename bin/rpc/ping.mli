(** [dune rpc ping] connects to a running RPC server and awaits a reply in
    order to determine if it is functioning correctly. Passing [--wait] allows
    for the command to wait for a connection if none are available. *)
val cmd : unit Cmdliner.Cmd.t
