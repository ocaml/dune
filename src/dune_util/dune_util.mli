module Action = Action
module Alias_name = Alias_name
module Build_path_prefix_map = Build_path_prefix_map0
module Gc = Gc
module Global_lock = Global_lock
module Log = Log
module Persistent = Persistent
module Report_error = Report_error
module Stringlike = Stringlike

module type Stringlike = Stringlike_intf.S

open Stdune

val xdg : Xdg.t Lazy.t
val override_xdg : Xdg.t -> unit

(** The default directory of all caches (build and others), used when
    environment variables are unset.
    Set to [$XDG_CACHE_HOME/dune]. *)
val default_cache_dir : Path.t Lazy.t

val check_absolute : var:string -> path:string -> unit

(** The cache for the git repository.
    Uses [$DUNE_CACHE_HOME/git-repo] if set, or
    [default_cache_dir/git-repo] otherwise. *)
val rev_store : Path.t Lazy.t

(** The cache for the toolchains.
    Uses [$DUNE_CACHE_HOME/toolchains] if set, or
    [default_cache_dir/toolchains] otherwise. *)
val toolchains_dir : Path.t Lazy.t

val frames_per_second : unit -> int
