open Stdune

type t

val make : build_dir:Path.t -> t

val read : t -> Package.t -> (unit, string option) Build.t

val rule
  :  t
  -> Package.t
  -> (unit, string option) Build.t
  -> (unit, Action.t) Build.t
