open Import

module Where : Dune_rpc.Where.S with type 'a fiber := 'a

val default : unit -> Dune_rpc.Where.t

val get : unit -> Dune_rpc.Where.t option

val to_socket : Dune_rpc.Where.t -> Unix.sockaddr

val to_string : Dune_rpc.Where.t -> string
