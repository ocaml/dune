open Stdune

module Files : sig
  type 'a t =
    { c   : 'a
    ; cxx : 'a
    }

  val make : files:String.Set.t -> String.Set.t t

  val foreign_objects
    :  (Loc.t * Path.t) String.Map.t t
    -> dir:Path.t
    -> ext_obj:string
    -> Path.t list
end

type t

val empty : t

val for_lib : t -> dir:Path.t -> name:Lib_name.t -> (Loc.t * Path.t) String.Map.t Files.t

val make
  :  Stanza.t list Dir_with_dune.t
  -> c_files:String.Set.t Files.t
  -> t
