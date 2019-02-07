open! Stdune

type t

val make
  :  Dune_file.Preprocess.t Dune_file.Per_module.t
  -> t

val pped_modules : t -> Module.Name_map.t -> Module.Name_map.t
