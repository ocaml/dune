open! Stdune
open! Import

type t =
  { name : Context_name.t
  ; build_dir : Path.Build.t
  ; env : Env.t
  ; ocaml : Action.Prog.t
  ; host : t option
  }

let create ~name ~build_dir ~env ~ocaml ~host =
  { name; build_dir; env; ocaml; host }
