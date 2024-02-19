open Import

type t

val url : t -> Loc.t * OpamUrl.t

module Package : sig end

module DB : sig
  type t

  val empty : t
  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool
  val hash : t -> int
  val decode : dir:Path.Source.t -> t Dune_lang.Decoder.fields_parser
  val encode : t -> Dune_lang.t list
  val combine_exn : t -> t -> t
  val add_opam_pins : t -> Dune_lang.Package.t Package_name.Map.t -> t

  module Workspace : sig
    type db := t
    type t

    val decode : t Dune_lang.Decoder.fields_parser
    val empty : t
    val extract : t -> names:(Loc.t * string) list -> db
    val equal : t -> t -> bool
    val to_dyn : t -> Dyn.t
    val hash : t -> int
  end
end

module Scan_project : sig
  type t =
    read:(Path.Source.t -> string Fiber.t)
    -> files:Filename.Set.t
    -> (DB.t * Dune_lang.Package.t Package_name.Map.t) option Fiber.t
end

val resolve
  :  DB.t
  -> scan_project:Scan_project.t
  -> Resolved_package.t Package_name.Map.t Fiber.t
