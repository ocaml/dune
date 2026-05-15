open Import

(** Create a bin-layout directory for the given binary names. Returns the
    layout directory and the list of symlink paths for dependency tracking.
    The symlinks are created as build rules keyed by a digest of the sorted
    binary names. *)
val create : Context_name.t -> string list -> (Path.Build.t * Path.t list) Memo.t

(** Generate symlink rules for the bin-layout directory identified by [key].
    Called from [gen_rules] when the build system visits
    [_build/install/<context>/.bin-layout/<key>/]. *)
val gen_rules : Context_name.t -> dir:Path.Build.t -> string -> unit Memo.t
