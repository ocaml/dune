open Import

(** [start ~name ~rpc_fd] starts a runner named [name] connected to the main
    dune process through [rpc_fd]. *)
val start : name:Action_runner_name.t -> rpc_fd:Fd.t -> unit Fiber.t
