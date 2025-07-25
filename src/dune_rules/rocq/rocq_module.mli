(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import

module Name : sig
  type t

  val make : string -> t
  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val to_string : t -> string
  val decode : t Dune_lang.Decoder.t

  module Map : Map.S with type key = t
end

type t

module Map : Map.S with type key = t

(** A Rocq module [a.b.foo] defined in file [a/b/foo.v] *)
val make
  :  source:Path.t (** file = .v source file; module name has to be the same so far *)
  -> prefix:string list (** Library-local qualified prefix *)
  -> name:Name.t (** Name of the module *)
  -> t

(** Rocq does enforce some invariants wrt module vs file names *)

val source : t -> Path.t
val prefix : t -> string list
val name : t -> Name.t
val dep_file : t -> obj_dir:Path.Build.t -> Path.Build.t
val glob_file : t -> obj_dir:Path.Build.t -> Path.Build.t

(** Some of the object files should not be installed, we control this with the
    following parameter *)
type obj_files_mode =
  | Build
  | Install

(** This returns a list of pairs [(obj_file, install_path)] due to native files
    having a different install location *)
val obj_files
  :  t
  -> wrapper_name:string
  -> mode:Rocq_mode.t
  -> obj_dir:Path.Build.t
  -> obj_files_mode:obj_files_mode
  -> (Path.Build.t * string) list

val to_dyn : t -> Dyn.t
val eval : dir:Path.Build.t -> standard:t list -> Ordered_set_lang.t -> t list
