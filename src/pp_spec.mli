(** This module is used to determine the extensions of preprocessed and reason
    sourced modules. *)
open! Stdune

type t

val make
  :  Dune_file.Preprocess.t Dune_file.Per_module.t
  -> t

val pped_modules : t -> Module.Name_map.t -> Module.Name_map.t
