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
  -> visible_packages:Package.Name.Set.t option Memo.t
  -> lockdir_bin_env:Env.t Memo.t
  -> t

val external_env : t -> Env.t Memo.t

(** [external_env] without the directories holding the binaries bound by
    [(env (binaries ...))]. An action merges its own environment into this and
    then adds [local_bin_dirs] on top, so that nothing staged by [(deps ...)]
    shadows a binary the user bound explicitly. *)
val env_without_local_bins : t -> Env.t Memo.t

(** Directories holding the binaries bound by [(env (binaries ...))], most
    specific first. *)
val local_bin_dirs : t -> Path.t list Memo.t

(** Binaries that are symlinked in the associated .bin directory of [dir]. *)
val local_binaries : t -> File_binding.Expanded.t list Memo.t

val artifacts : t -> Artifacts.t Memo.t
