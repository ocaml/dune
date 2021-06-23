open Import

val default : unit -> Dune_rpc.Where.t

val get : unit -> Dune_rpc.Where.t option

val to_socket : Dune_rpc.Where.t -> Unix.sockaddr
