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
val do_build
  :  t
  -> request:(unit, unit) Build.t
  -> (unit Future.t, Build_error.t) result
val do_build_exn
  :  t
  -> request:(unit, unit) Build.t
  -> unit Future.t

(** Return all the library dependencies (as written by the user)
   needed to build this request *)
val all_lib_deps
  :  t
  -> request:(unit, unit) Build.t
  -> Build.lib_deps Path.Map.t

(** Return all the library dependencies required to build this
   request, by context name *)
val all_lib_deps_by_context
  :  t
  -> request:(unit, unit) Build.t
  -> Build.lib_deps String_map.t

(** List of all buildable targets *)
val all_targets : t -> Path.t list

(** A fully built rule *)
module Rule : sig
  module Id : sig
    type t
    val to_int : t -> int
    val compare : t -> t -> int
  end

  type t =
    { id      : Id.t
    ; deps    : Path.Set.t
    ; targets : Path.Set.t
    ; context : Context.t option
    ; action  : Action.t
    }
end

(** Return the list of rules used to build the given targets. If
    [recursive] is [true], return all the rules needed to build the
    given targets and their transitive dependencies. *)
val build_rules
  :  ?recursive:bool (* default false *)
  -> t
  -> request:(unit, unit) Build.t
  -> Rule.t list Future.t

val all_targets_ever_built
  :  unit
  -> Path.t list

val dump_trace : t -> unit
