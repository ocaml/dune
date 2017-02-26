(** The build arrow *)

open! Import

type ('a, 'b) t

val arr : ('a -> 'b) -> ('a, 'b) t

val return : 'a -> (unit, 'a) t

val create_file : target:Path.t -> ('a -> 'b) -> ('a, 'b) t
val create_files : targets:Path.t list -> ('a -> 'b) -> ('a, 'b) t

module Vspec : sig
  type 'a t = T : Path.t * 'a Vfile_kind.t -> 'a t
end

val store_vfile : 'a Vspec.t -> ('a, unit) t

module O : sig
  val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val ( ^>> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val ( >>^ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  val ( *** ) : ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  val ( &&& ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
end

val first  : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
val second : ('a, 'b) t -> ('c * 'a, 'c * 'b) t

(** Same as [O.(&&&)]. Sends the input to both argument arrows and combine their output.

    The default definition may be overridden with a more efficient version if desired. *)
val fanout  : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
val fanout3 : ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t ->  ('a, 'b * 'c * 'd) t

val all : ('a, 'b) t list -> ('a, 'b list) t

val path  : Path.t      -> ('a, 'a) t
val paths : Path.t list -> ('a, 'a) t
val path_set : Path.Set.t -> ('a, 'a) t
val paths_glob : dir:Path.t -> Re.re -> ('a, 'a) t
val files_recursively_in : dir:Path.t -> ('a, 'a) t
val vpath : 'a Vspec.t  -> (unit, 'a) t

val dyn_paths : ('a, Path.t list) t -> ('a, 'a) t

(** Always fail when executed. We pass a function rather than an exception to get a proper
    backtrace *)
val fail : fail -> ('a, 'a) t

module Prog_spec : sig
  type 'a t =
    | Dep of Path.t
    | Dyn of ('a -> Path.t)
end

val run
  :  ?dir:Path.t
  -> ?stdout_to:Path.t
  -> ?env:string array
  -> ?extra_targets:Path.t list
  -> 'a Prog_spec.t
  -> 'a Arg_spec.t list
  -> ('a, unit) t

val run_capture
  :  ?dir:Path.t
  -> ?env:string array
  -> 'a Prog_spec.t
  -> 'a Arg_spec.t list
  -> ('a, string) t

val run_capture_lines
  :  ?dir:Path.t
  -> ?env:string array
  -> 'a Prog_spec.t
  -> 'a Arg_spec.t list
  -> ('a, string list) t

val action : targets:Path.t list -> (Action.t, unit) t

(** Create a file with the given contents. *)
val echo : Path.t -> (string, unit) t

val copy : src:Path.t -> dst:Path.t -> (unit, unit) t

val touch : Path.t -> (unit, unit) t

type lib_dep_kind =
  | Optional
  | Required

val record_lib_deps
  :  dir:Path.t
  -> kind:lib_dep_kind
  -> Jbuild_types.Lib_dep.t list
  -> ('a, 'a) t

type lib_deps = lib_dep_kind String_map.t

(**/**)


module Repr : sig
  type ('a, 'b) prim =
    { targets : Path.t list
    ; exec    : 'a -> 'b Future.t
    }
  type ('a, 'b) t =
    | Arr : ('a -> 'b) -> ('a, 'b) t
    | Prim : ('a, 'b) prim -> ('a, 'b) t
    | Store_vfile : 'a Vspec.t -> ('a, unit) t
    | Compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
    | First : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
    | Second : ('a, 'b) t -> ('c * 'a, 'c * 'b) t
    | Split : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
    | Fanout : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
    | Paths : Path.Set.t -> ('a, 'a) t
    | Paths_glob : Path.t * Re.re -> ('a, 'a) t
    | Vpath : 'a Vspec.t -> (unit, 'a) t
    | Dyn_paths : ('a, Path.t list) t -> ('a, 'a) t
    | Record_lib_deps : Path.t * lib_deps -> ('a, 'a) t
    | Fail : fail -> ('a, 'a) t
end

val repr : ('a, 'b) t -> ('a, 'b) Repr.t

val merge_lib_deps : lib_deps -> lib_deps -> lib_deps
