(** Build rules *)

open! Import

type t

val create
  :  contexts:Context.t list
  -> file_tree:File_tree.t
  -> rules:Build_interpret.Rule.t list
  -> t

val is_target : t -> Path.t -> bool

module Build_error : sig
  type t

  val backtrace : t -> Printexc.raw_backtrace
  val dependency_path : t -> Path.t list
  val exn : t -> exn

  exception E of t
end

(** Do the actual build *)
val do_build     : t -> Path.t list -> (unit Future.t, Build_error.t) result
val do_build_exn : t -> Path.t list -> unit Future.t

(** Return all the library dependencies (as written by the user) needed to build these
    targets *)
val all_lib_deps : t -> Path.t list -> Build.lib_deps Path.Map.t

(** Return all the library dependencies required to build these targets, by context
    name *)
val all_lib_deps_by_context : t -> Path.t list -> Build.lib_deps String_map.t
