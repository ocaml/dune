open Import

(** [start ~name ~generation ~where] starts a runner named [name] connected to
    the main dune RPC server listening at [where]. *)
val start
  :  name:Action_runner_name.t
  -> generation:int
  -> where:Dune_rpc.Where.t
  -> unit Fiber.t
