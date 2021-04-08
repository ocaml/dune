open! Stdune
open! Import

type t =
  { name : Context_name.t
  ; build_dir : Path.Build.t
  ; host : Context_name.t option
  }

let create ~name ~host =
  let build_dir = Path.Build.of_string (Context_name.to_string name) in
  { name; build_dir; host }
