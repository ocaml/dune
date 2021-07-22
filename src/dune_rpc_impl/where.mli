open Import

module Where : Dune_rpc.Where.S with type 'a fiber := 'a

val default : unit -> Dune_rpc.Where.t

val get : unit -> Dune_rpc.Where.t option

val get_ignore_env : unit -> Dune_rpc.Where.t option

val to_socket : Dune_rpc.Where.t -> Unix.sockaddr
