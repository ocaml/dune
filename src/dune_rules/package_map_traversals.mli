module Map := Package.Name.Map

type t := Package.Name.t

val parallel_map : 'a Map.t -> f:(t -> 'a -> 'b Memo.t) -> 'b Map.t Memo.t
