open Import

(** Restrict the set of visible packages *)

module Clflags : sig
  type t =
    | No_restriction
    | Restrict of
        { names : Package.Name.Set.t
        ; command_line_option : string
          (** Which of [-p], [--only-packages], ... was passed *)
        }

  val equal : t -> t -> bool

  (** This must be called exactly once *)
  val set : t -> unit
end

type t

val enumerate : t -> [ `Set of Package.Name.Set.t | `All ]
val mem : t -> Package.Name.t -> bool
val mem_all : t -> bool
val mask : (Package.t * Package.status) list Package.Name.Map.t -> t
val filter_packages : t -> 'a Package.Name.Map.t -> 'a Package.Name.Map.t
val filter_packages_in_project : vendored:bool -> Dune_project.t -> Dune_project.t
