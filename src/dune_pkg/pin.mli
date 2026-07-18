open Import

module DB : sig
  type t

  module Pin_stanza := Dune_lang.Pin_stanza

  val empty : t
  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool
  val hash : t -> int
  val of_stanza : dir:Path.Source.t -> Pin_stanza.Project.t -> t
  val combine_exn : t -> t -> t
  val add_opam_pins : t -> Dune_lang.Package.t Package_name.Map.t -> t

  (** [filter_by_package_names t ~package_names] returns a new pin database
      containing only the pins for packages whose names are in [package_names]. *)
  val filter_compilers : t -> t

  module Workspace : sig
    type db := t
    type t

    val of_stanza : Pin_stanza.Workspace.t -> t
    val empty : t
    val extract : t -> names:(Loc.t * string) list -> db
    val extract_and_combine : t -> names:(Loc.t * string) list -> project_pins:db -> db
    val equal : t -> t -> bool
    val to_dyn : t -> Dyn.t
    val hash : t -> int
  end
end

module Scan_project : sig
  type t =
    read:(Path.Source.t -> string Fiber.t)
    -> files:Filename.Array.Set.t
    -> (DB.t * Dune_lang.Package.t Package_name.Map.t) option Fiber.t
end

module Project : sig
  val collect_all : Dune_lang.Dune_project.t list -> DB.t
  val resolve : DB.t -> Resolved_package.t Package_name.Map.t Fiber.t
end
