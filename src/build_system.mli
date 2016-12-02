(** Build rules *)

open Import

module Vspec : sig
  type 'a t = T : Path.t * 'a Vfile_kind.t -> 'a t
end

module Prog_spec : sig
  type 'a t =
    | Dep of Path.t
    | Dyn of ('a -> Path.t)
end

module Build : sig
  (** The build arrow *)
  type ('a, 'b) t

  val arr : ('a -> 'b) -> ('a, 'b) t

  val return : 'a -> (unit, 'a) t

  val create_file : target:Path.t -> ('a -> 'b) -> ('a, 'b) t
  val create_files : targets:Path.t list -> ('a -> 'b) -> ('a, 'b) t
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

      The default definition may be overridden with a more efficient version if
      desired. *)
  val fanout  : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  val fanout3 : ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t ->  ('a, 'b * 'c * 'd) t

  val all : ('a, 'b) t list -> ('a, 'b list) t

  val path  : Path.t      -> ('a, 'a) t
  val paths : Path.t list -> ('a, 'a) t
  val path_set : Path.Set.t -> ('a, 'a) t
  val vpath : 'a Vspec.t  -> (unit, 'a) t

  val dyn_paths : ('a, Path.t list) t -> ('a, 'a) t

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

  val record_lib_deps : string list -> ('a, 'a) t
end

val rule : (unit, unit) Build.t -> unit

val copy_rule : src:Path.t -> dst:Path.t -> unit

module Build_error : sig
  type t

  val backtrace : t -> Printexc.raw_backtrace
  val dependency_path : t -> Path.t list
  val exn : t -> exn

  exception E of t
end

(** Do the actual build *)
val do_build     : Path.t list -> (unit Future.t, Build_error.t) result
val do_build_exn : Path.t list -> unit Future.t

(** Return all the library dependencies (as written by the user) needed to build these
    targets *)
val all_lib_deps : Path.t list -> String_set.t
