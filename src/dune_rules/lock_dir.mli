open Import
module Pkg = Dune_pkg.Lock_dir.Pkg

type t := Dune_pkg.Lock_dir.t

val get : Context_name.t -> t Memo.t
val lock_dir_active : Context_name.t -> bool Memo.t
val get_path : Context_name.t -> Path.Source.t option Memo.t
