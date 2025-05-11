open Import

module File : sig
  type t

  val dummy : t

  val of_source_path
    :  Path.Outside_build_dir.t
    -> (t, Unix_error.Detailed.t) result Memo.t

  module Map : Map.S with type key = t
end

type t

val empty : t
val dirs : t -> (Filename.t * File.t) list
val files : t -> Filename.Set.t
val to_dyn : t -> Dyn.t
val of_source_path : Path.Source.t -> (t, Unix_error.Detailed.t) result Memo.t
