open Import

type t

val make :
  dir:Path.t ->
  inherit_from:t Lazy.t option ->
  scope:Scope.t ->
  config:Dune_env.Stanza.t ->
  t

val ocaml_flags :
  t ->
  profile:string ->
  eval:(scope:Scope.t ->
        dir:Path.t ->
        Ordered_set_lang.Unexpanded.t ->
        standard:(unit, string list) Build.t -> (unit, string list) Build.t) ->
  Ocaml_flags.t
