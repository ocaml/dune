(** An environment node represents an evaluated (env ..) stanza in a directory. *)

open Import

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

val make
  :  Build_context.t
  -> dir:Path.Build.t
  -> inherit_from:t Memo.Lazy.t option
  -> scope:Scope.t
  -> config_stanza:Dune_env.Stanza.t
  -> profile:Profile.t
  -> expander:Expander.t Memo.Lazy.t
  -> expander_for_artifacts:Expander.t Memo.Lazy.t
  -> default_context_flags:string list Action_builder.t Foreign_language.Dict.t
  -> default_env:Env.t
  -> default_artifacts:Artifacts.t
  -> default_bin_annot:bool
  -> t

val scope : t -> Scope.t
val external_env : t -> Env.t Memo.t
val ocaml_flags : t -> Ocaml_flags.t Memo.t
val inline_tests : t -> Dune_env.Stanza.Inline_tests.t Memo.t
val js_of_ocaml : t -> string list Action_builder.t Js_of_ocaml.Env.t Memo.t
val foreign_flags : t -> string list Action_builder.t Foreign_language.Dict.t
val link_flags : t -> Link_flags.t Memo.t

(** Binaries that are symlinked in the associated .bin directory of [dir]. This
    associated directory is *)
val local_binaries : t -> File_binding.Expanded.t list Memo.t

val artifacts : t -> Artifacts.t Memo.t
val odoc : t -> Odoc.t Action_builder.t
val coq : t -> Coq.t Action_builder.t Memo.t
val menhir_flags : t -> string list Action_builder.t
val format_config : t -> Format_config.t Memo.t
val set_format_config : t -> Format_config.t -> t
val bin_annot : t -> bool Memo.t
