open Stdune

type t

val empty : t

val for_lib : t -> dir:Path.t -> name:Lib_name.t -> C.Sources.t

val load_sources
  :  dir:Path.t
  -> files:String.Set.t
  -> C.Source.t String.Map.t C.Kind.Dict.t

val make
  :  Stanza.t list Dir_with_dune.t
  -> c_sources:C.Source.t String.Map.t C.Kind.Dict.t
  -> t
