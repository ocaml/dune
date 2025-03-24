(** An environment node represents an evaluated (env ..) stanza in a directory. *)

open Import

type t

val make
  :  dir:Path.Build.t
  -> inherit_from:t Memo.Lazy.t option
  -> config_stanza:Dune_env.t
  -> profile:Profile.t
  -> expander:Expander.t Memo.t
  -> default_env:Env.t Memo.t
  -> default_artifacts:Artifacts.t Memo.t
  -> t

val external_env : t -> Env.t Memo.t

(** Binaries that are symlinked in the associated .bin directory of [dir]. This
    associated directory is *)
val local_binaries : t -> File_binding.Expanded.t list Memo.t

val artifacts : t -> Artifacts.t Memo.t
