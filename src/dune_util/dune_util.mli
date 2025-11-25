module Action = Action
module Alias_name = Alias_name
module Build_path_prefix_map = Build_path_prefix_map0
module Gc = Gc
module Global_lock = Global_lock
module Persistent = Persistent
module Report_error = Report_error
module Stringlike = Stringlike

module type Stringlike = Stringlike_intf.S

open Stdune

val xdg : Xdg.t Lazy.t
val override_xdg : Xdg.t -> unit

(** The directory containing all caches (build and others).
    Set to [$DUNE_CACHE_HOME] if it exists, or
    [$XDG_CACHE_HOME/dune] otherwise. *)
val cache_home_dir : Path.t Lazy.t

val frames_per_second : unit -> int
