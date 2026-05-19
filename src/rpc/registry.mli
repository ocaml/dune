open Import

type t

val create : root:string -> where:Dune_rpc.Where.t -> [ `Add | `Skip ] -> t
val register : t -> unit
val cleanup : t -> unit
