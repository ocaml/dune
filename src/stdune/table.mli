module type Key = sig type t val equal : t -> t -> bool val hash : t -> int end

type ('k, 'v) t

val create : (module Key with type t = 'k) -> int -> ('k, 'v) t

val find : ('k, 'v) t -> 'k -> 'v option

val add : ('k, 'v) t -> 'k -> 'v -> unit
