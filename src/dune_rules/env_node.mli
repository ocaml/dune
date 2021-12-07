(** An environment node represents an evaluated (env ..) stanza in a directory. *)
open! Dune_engine

open Stdune

module Odoc : sig
  type warnings = Dune_env.Stanza.Odoc.warnings =
    | Fatal
    | Nonfatal

  type t = { warnings : warnings }
end

module Coq : sig
  type t = string list
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
  -> default_context_flags:string list Action_builder.t Foreign_language.Dict.t
  -> default_env:Env.t
  -> default_bin_artifacts:Artifacts.Bin.t
  -> default_cxx_link_flags:string list Action_builder.t
  -> t

val scope : t -> Scope.t

val external_env : t -> Env.t Memo.Build.t

val ocaml_flags : t -> Ocaml_flags.t Memo.Build.t

val inline_tests : t -> Dune_env.Stanza.Inline_tests.t Memo.Build.t

val js_of_ocaml :
  t -> string list Action_builder.t Js_of_ocaml.Env.t Memo.Build.t

val foreign_flags : t -> string list Action_builder.t Foreign_language.Dict.t

val link_flags : t -> Link_flags.t Memo.Build.t

val local_binaries : t -> File_binding.Expanded.t list Memo.Build.t

val bin_artifacts : t -> Artifacts.Bin.t Memo.Build.t

val odoc : t -> Odoc.t Memo.Build.t

val coq : t -> Coq.t Action_builder.t Memo.Build.t

val menhir_flags : t -> string list Action_builder.t

val format_config : t -> Format_config.t Memo.Build.t

val set_format_config : t -> Format_config.t -> t
