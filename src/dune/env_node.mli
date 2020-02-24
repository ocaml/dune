(** An environment node represents an evaluated (env ..) stanza in a directory. *)

open Stdune

module Odoc : sig
  type warnings = Dune_env.Stanza.Odoc.warnings =
    | Fatal
    | Nonfatal

  type t = { warnings : warnings }
end

type t

val make :
     dir:Path.Build.t
  -> inherit_from:t Memo.Lazy.t option
  -> scope:Scope.t
  -> config_stanza:Dune_env.Stanza.t
  -> profile:Profile.t
  -> expander:Expander.t Memo.Lazy.t
  -> expander_for_artifacts:Expander.t Memo.Lazy.t
  -> default_context_flags:string list Foreign.Language.Dict.t
  -> default_env:Env.t
  -> default_bin_artifacts:Artifacts.Bin.t
  -> t

val scope : t -> Scope.t

val external_env : t -> Env.t

val ocaml_flags : t -> Ocaml_flags.t

val inline_tests : t -> Dune_env.Stanza.Inline_tests.t

val foreign_flags : t -> string list Build.t Foreign.Language.Dict.t

val local_binaries : t -> File_binding.Expanded.t list

val bin_artifacts : t -> Artifacts.Bin.t

val odoc : t -> Odoc.t

val menhir_flags : t -> string list Build.t
