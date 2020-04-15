(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune

type t

val name : t -> Coq_lib_name.t

val implicit : t -> bool

(* this is not really a wrapper for the prefix path *)
val wrapper : t -> string

(** ml libraries *)
val libraries : t -> (Loc.t * Lib_name.t) list

val src_root : t -> Path.Build.t

val obj_root : t -> Path.Build.t

val package : t -> Package.t option

module DB : sig
  type lib

  type t

  val create_from_coqlib_stanzas :
    (Path.Build.t * Coq_stanza.Theory.t) list -> t

  val find_many : t -> loc:Loc.t -> Coq_lib_name.t list -> lib list Or_exn.t

  val boot_library : t -> (Loc.t * lib) option

  val resolve : t -> Loc.t * Coq_lib_name.t -> lib Or_exn.t

  (** Return the list of dependencies needed for compiling this library *)
  val requires : t -> lib -> lib list Or_exn.t

  val requires_for_user_written :
    t -> (Loc.t * Coq_lib_name.t) list -> lib list Or_exn.t
end
with type lib := t
