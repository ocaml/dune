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
  -> visible_packages:Package.Name.Selection.t Memo.t
  -> lockdir_bin_paths:Path.t list Memo.t
  -> t

val external_env : t -> Env.t Memo.t

(** The directory environment and the local binary directories to prepend
    after merging the action's own environment. *)
val action_env : t -> (Env.t * Path.t list) Action_builder.t

(** Binaries that are symlinked in the associated .bin directory of [dir]. *)
val local_binaries : t -> File_binding.Expanded.t list Memo.t

val artifacts : t -> Artifacts.t Memo.t
