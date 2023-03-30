open Stdune

let inside_emacs = Option.is_some (Env.get Env.initial "INSIDE_EMACS")

let inside_dune = Option.is_some (Env.get Env.initial "INSIDE_DUNE")

let inside_ci = Option.is_some (Env.get Env.initial "CI")
