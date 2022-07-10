open Import

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

module Name : sig
  type t

  val make : string -> t

  val equal : t -> t -> bool

  val to_dyn : t -> Dyn.t

  module Map : Map.S with type key = t
end

module Path : sig
  type t

  val empty : t

  val to_list : t -> Name.t list

  (** Returns true if there is at least one segment of the prefix that is
      shared. For instance A.B.C and A.B aggree*)
  val is_prefix : t -> prefix:t -> bool

  val to_dyn : t -> Dyn.t

  val append_name : t -> Name.t -> t

  val rev : t -> t

  val of_string : string -> t

  val of_string_list : string list -> t

  val to_string_list : t -> string list

  val of_lib_name : Coq_lib_name.t -> t

  module Map : Map.S with type key = t
end

module Source : sig
  type t

  val make : source:Stdune.Path.Build.t -> prefix:Path.t -> name:Name.t -> t
end

type t

module Map : Map.S with type key = t

(** A Coq module [a.b.foo] defined in file [a/b/foo.v] *)
val make :
     source:Stdune.Path.Build.t
       (** file = .v source file; module name has to be the same so far *)
  -> prefix:Path.t (** Library-local qualified prefix *)
  -> name:Name.t (** Name of the module *)
  -> theory_prefix:Path.t
  -> obj_dir:Stdune.Path.Build.t
  -> t

(* Change to take [Source.t] *)
val of_source :
  Source.t -> obj_dir:Stdune.Path.Build.t -> theory:Coq_lib_name.t -> t

(** Coq does enforce some invariants wrt module vs file names *)

val prefix : t -> Path.t

val name : t -> Name.t

val path : t -> Path.t

val obj_dir : t -> Stdune.Path.Build.t

val theory_prefix : t -> Path.t

val source : t -> Stdune.Path.Build.t

val dep_file : t -> Stdune.Path.Build.t

val glob_file : t -> Stdune.Path.Build.t

val vo_file : t -> Stdune.Path.Build.t

type target =
  | Vo
  | Vos

(** Some of the object files should not be installed, we control this with the
    following parameter *)
type obj_files_mode =
  | Build of target
  | Install

(** This returns a list of pairs [(obj_file, install_path)] due to native files
    having a different install location *)
val obj_files :
     t
  -> wrapper_name:string (* TODO: remove *)
  -> mode:Coq_mode.t
  -> obj_files_mode:obj_files_mode
  -> (Stdune.Path.Build.t * string) list

val to_dyn : t -> Dyn.t

val equal : t -> t -> bool

val eval :
     dir:Stdune.Path.Build.t
  -> standard:t list
  -> theory_prefix:Path.t
  -> obj_dir:Stdune.Path.Build.t
  -> Ordered_set_lang.t
  -> t list
