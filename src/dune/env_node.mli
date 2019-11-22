(** An environment node represents an evaluated (env ..) stanza in a directory. *)

open Stdune

type t

val make :
     dir:Path.Build.t
  -> inherit_from:t Lazy.t option
  -> scope:Scope.t
  -> config:Dune_env.Stanza.t
  -> t

val scope : t -> Scope.t

val external_ : t -> profile:Profile.t -> default:Env.t -> Env.t

val ocaml_flags : t -> profile:Profile.t -> expander:Expander.t -> Ocaml_flags.t

val inline_tests : t -> profile:Profile.t -> Dune_env.Stanza.Inline_tests.t

val foreign_flags :
     t
  -> profile:Profile.t
  -> expander:Expander.t
  -> default_context_flags:string list Foreign.Language.Dict.t
  -> string list Build.t Foreign.Language.Dict.t

val local_binaries :
  t -> profile:Profile.t -> expander:Expander.t -> File_binding.Expanded.t list

val bin_artifacts :
     t
  -> profile:Profile.t
  -> default:Artifacts.Bin.t
  -> expander:Expander.t
  -> Artifacts.Bin.t
