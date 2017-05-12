(** The build arrow *)

open! Import

type ('a, 'b) t

val arr : ('a -> 'b) -> ('a, 'b) t

val return : 'a -> (unit, 'a) t

module Vspec : sig
  type 'a t = T : Path.t * 'a Vfile_kind.t -> 'a t
end

val store_vfile : 'a Vspec.t -> ('a, Action.t) t

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
val files_recursively_in : dir:Path.t -> file_tree:File_tree.t -> ('a, 'a) t
val vpath : 'a Vspec.t  -> (unit, 'a) t

val dyn_paths : ('a, Path.t list) t -> ('a, 'a) t

val contents : Path.t -> ('a, string) t
val lines_of : Path.t -> ('a, string list) t

(** Evaluates to [true] if the file is present on the file system or is the target of a
    rule. *)
val file_exists : Path.t -> ('a, bool)  t

(** [if_file_exists p ~then ~else] is an arrow that behaves like [then_] if [file_exists
    p] evaluates to [true], and [else_] otherwise. *)
val if_file_exists : Path.t -> then_:('a, 'b) t -> else_:('a, 'b) t -> ('a, 'b) t

(** [file_exists_opt p t] is:

    {[
      if_file_exists p ~then_:(t >>^ fun x -> Some x) ~else_:(arr (fun _ -> None))
    ]}
*)
val file_exists_opt : Path.t -> ('a, 'b) t -> ('a, 'b option) t

(** Always fail when executed. We pass a function rather than an exception to get a proper
    backtrace *)
val fail : ?targets:Path.t list -> fail -> (_, _) t

module Prog_spec : sig
  type 'a t =
    | Dep of Path.t
    | Dyn of ('a -> Path.t)
end

val run
  :  context:Context.t
  -> ?dir:Path.t (* default: context.build_dir *)
  -> ?stdout_to:Path.t
  -> ?extra_targets:Path.t list
  -> 'a Prog_spec.t
  -> 'a Arg_spec.t list
  -> ('a, Action.t) t

val action
  :  context:Context.t
  -> ?dir:Path.t (* default: context.build_dir *)
  -> targets:Path.t list
  -> Action.Mini_shexp.t
  -> (unit, Action.t) t

val action_dyn
  :  context:Context.t
  -> ?dir:Path.t (* default: context.build_dir *)
  -> targets:Path.t list
  -> unit
  -> (Action.Mini_shexp.t, Action.t) t

val action_context_independent
  :  ?dir:Path.t (* default: Path.root *)
  -> targets:Path.t list
  -> Action.Mini_shexp.t
  -> (unit, Action.t) t

(** Create a file with the given contents. Do not ovewrite the file if
    it hasn't changed. *)
val update_file : Path.t -> string -> (unit, Action.t) t
val update_file_dyn : Path.t -> (string, Action.t) t

val copy : src:Path.t -> dst:Path.t -> (unit, Action.t) t

val symlink : src:Path.t -> dst:Path.t -> (unit, Action.t) t

val create_file : Path.t -> (unit, Action.t) t

val and_create_file : Path.t -> (Action.t, Action.t) t

type lib_dep_kind =
  | Optional
  | Required

val record_lib_deps
  :  dir:Path.t
  -> kind:lib_dep_kind
  -> Jbuild_types.Lib_dep.t list
  -> ('a, 'a) t

type lib_deps = lib_dep_kind String_map.t

val record_lib_deps_simple : dir:Path.t -> lib_deps -> ('a, 'a) t

(**/**)


module Repr : sig
  type ('a, 'b) t =
    | Arr : ('a -> 'b) -> ('a, 'b) t
    | Targets : Path.t list -> ('a, 'a) t
    | Store_vfile : 'a Vspec.t -> ('a, Action.t) t
    | Compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
    | First : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
    | Second : ('a, 'b) t -> ('c * 'a, 'c * 'b) t
    | Split : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
    | Fanout : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
    | Paths : Path.Set.t -> ('a, 'a) t
    | Paths_glob : Path.t * Re.re -> ('a, 'a) t
    | If_file_exists : Path.t * ('a, 'b) if_file_exists_state ref -> ('a, 'b) t
    | Contents : Path.t -> ('a, string) t
    | Lines_of : Path.t -> ('a, string list) t
    | Vpath : 'a Vspec.t -> (unit, 'a) t
    | Dyn_paths : ('a, Path.t list) t -> ('a, 'a) t
    | Record_lib_deps : Path.t * lib_deps -> ('a, 'a) t
    | Fail : fail -> (_, _) t

  and ('a, 'b) if_file_exists_state =
    | Undecided of ('a, 'b) t * ('a, 'b) t
    | Decided   of bool * ('a, 'b) t

  val get_if_file_exists_exn : ('a, 'b) if_file_exists_state ref -> ('a, 'b) t
end

val repr : ('a, 'b) t -> ('a, 'b) Repr.t

val merge_lib_deps : lib_deps -> lib_deps -> lib_deps
