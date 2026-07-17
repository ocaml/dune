open Import

(** [create context ~dir bin_names] creates a .binaries directory for the
    [%{bin:...}] names resolved from [dir], covering the names that resolve to
    local package binaries. [context] must be the host context in which the
    binaries run. Returns [None] when no name does. Otherwise returns the
    directory and the list of symlink paths for dependency tracking; depend on
    the paths before putting the directory on [PATH]. The symlinks are created
    as build rules keyed by a digest of the sorted (lookup name, installed
    filename) pairs. *)
val create
  :  Context_name.t
  -> dir:Path.Build.t
  -> string list
  -> (Path.Build.t * Path.t list) option Memo.t

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
