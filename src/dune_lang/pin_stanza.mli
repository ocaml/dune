open Import

module Package : sig
  type t =
    { version : Package_version.t option
    ; name : Package_name.t
    ; loc : Loc.t
    }
end

type t =
  { url : Loc.t * Url.t
  ; packages : Package.t list
  }

val package_map_of_list
  :  (Package_name.t * 'a) list
  -> loc:('a -> Loc.t)
  -> 'a Package_name.Map.t

module Project : sig
  type pin := t
  type nonrec t

  val empty : t
  val all : t -> pin list
  val map : t -> ((Loc.t * Url.t) * Package.t) Package_name.Map.t
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val hash : t -> int
  val decode : t Decoder.fields_parser
  val encode : t -> Dune_sexp.t list
end

module Workspace : sig
  type nonrec t

  val map : t -> ((Loc.t * Url.t) * Package.t) Package_name.Map.t String.Map.t
  val empty : t
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val hash : t -> int
  val decode : t Decoder.fields_parser
end
