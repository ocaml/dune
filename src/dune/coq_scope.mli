(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2019                    *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune

type t

(** Return the library database associated to this scope *)
val libs : t -> Coq_lib.DB.t

module DB : sig
  type scope = t

  type t

  val create :
    context:Context_name.t -> (Path.Build.t * Dune_file.Coq.t) list -> t

  val find_by_dir : t -> Path.Build.t -> scope

  val find_by_project : t -> Dune_project.t -> scope
end
with type scope := t
