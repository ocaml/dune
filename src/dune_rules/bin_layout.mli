open Import

(** Create a .binaries directory for the given binary names. Returns the
    directory and the list of symlink paths for dependency tracking. The
    symlinks are created as build rules keyed by a digest of the sorted
    binary names. *)
val create : Context_name.t -> string list -> Path.Build.t * Path.t list

(** Dispatch a path under [_build/install/<context>/.binaries/...].
    [rest] is the components after [.binaries].
    - [[]]: at the [.binaries/] directory itself, no rules but the
      engine descends.
    - [[ key ]]: produce symlink rules for the bin set keyed by [key].
    - deeper: redirect to parent. *)
val gen_rules
  :  Context_name.t
  -> dir:Path.Build.t
  -> string list
  -> Build_config.Gen_rules.result Memo.t
