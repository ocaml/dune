open! Stdune
open! Import

type t =
  { name : Context_name.t
  ; build_dir : Path.Build.t
  ; env : Env.t
  ; host : t option
  }

let create ~name ~build_dir ~env ~host = { name; build_dir; env; host }
