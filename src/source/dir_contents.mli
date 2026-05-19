open Import

module File : sig
  type t

  val dummy : t
  val of_source_path : Path.Source.t -> (t, Unix_error.Detailed.t) result Memo.t

  module Map : Map.S with type key = t
end

type t

val empty : t
val dirs : t -> File.t Filename.Array.Map.t
val files : t -> Filename.Array.Set.t
val to_dyn : t -> Dyn.t
val of_source_path : Path.Source.t -> (t, Unix_error.Detailed.t) result Memo.t
