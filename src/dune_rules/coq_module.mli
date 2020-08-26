open! Dune_engine

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune

module Name : sig
  type t

  val make : string -> t

  val compare : t -> t -> Ordering.t

  val equal : t -> t -> bool

  val to_dyn : t -> Dyn.t

  val to_string : t -> string
end

type t

(** A Coq module [a.b.foo] defined in file [a/b/foo.v] *)
val make :
     source:Path.Build.t
       (** file = .v source file; module name has to be the same so far *)
  -> prefix:string list (** Library-local qualified prefix *)
  -> name:Name.t (** Name of the module *)
  -> t

(** Coq does enforce some invariants wrt module vs file names *)

val source : t -> Path.Build.t

val prefix : t -> string list

val name : t -> Name.t

type obj =
  | Dep
  | Aux
  | Glob
  | Obj

val obj_file : t -> obj -> obj_dir:Path.Build.t -> Path.Build.t

val to_dyn : t -> Dyn.t

val eval : dir:Path.Build.t -> standard:t list -> Ordered_set_lang.t -> t list
